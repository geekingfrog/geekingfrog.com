---
title: ZScaler and corporate MITM
status: published
tags:
- security
- networking
- TIL
---

# What is zscaler?

For $dayjob, I have a corporate laptop, fairly locked down and full of spyware installed by corporate IT, allegedly to make sure I'm not being naughty. Today I want to talk about a specific software: zscaler, a corporate tool for "data protection". You can search their online presence, they're very enterprise ready™.

# How does it work?

On my machine I have a daemon that intercept almost all `https` requests. This should not be possible since `tls`, the mechanism to secure a connection is end to end, and designed to prevent such things.

So instead, zscaler performs what's known as a [man in the middle attack](https://en.wikipedia.org/wiki/Man-in-the-middle_attack).
It terminates any tls connection, pretending to be the application, and then present to the actual application a new certificate. This works because the zscaler agent also install a zscaler root certificate in the certificate authority store of the operating system, making itself one of the trusted root authority.

So for example, when an application makes a request to gitlab.com this is roughly what happens:

```
application             zscaler   OS root CA store          gitlab.com
    |                      |                                     |
    | −−−−−−−CHLO−−−−−−−−> | −−−−−−−−−−−−−−−−CHLO(Z)−−−−−−−−−−−> |
    |                      |                                     |
    | <−−−−−−SHLO(Z)−−−−−− | <−−−−−−−−−−−−−−−SHLO−−−−−−−−−−−−−−− |
    |
    | −−−−−−−−−− zscaler trusted CA? −−> |
    |                                    |
    | <−−−− yup, carry on −−−−−−−−−−−−−− |
    .
    .
    .
    (continue connection as normal)
```

So overall, it just works, and your corporate master has the ability to know everything you do on their network.


# The wrinkle

The corporate machine is running windows, but I'm doing all my dev work in a linux VM. And the linux VM does NOT have the zscaler agent installed and running. So most requests are rejected because the operating system doesn't recognize `zscaler.com` as a valid root CA. The modified certificates signed by the agent don't work anymore.


# "Fixing" things

So the workaround is to add the zscaler root CA certificate to the OS trust store. The following works with archlinux. For other distros like ubuntu, the mechanism is a bit different, but the internet has the answer.

```
openssl s_client -showcerts -connect gitlab.com:443 < /dev/null \
  | /usr/bin/sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' \
  > /tmp/ca.crt
```

`openssl s_client` is a tool to establish a TLS connection to a given host + port. Here, I'm asking it to also dump the certificates presented by the server during connection establishment.
The connection will fail because of the trust issue mentioned above, but that's not a problem here.

The `sed` command grab all the certificates, big base64 encoded blob of data, between known markers.

And then, we can tell the OS to grab and add these certificates to the root CA store:
```
trust anchor --store /tmp/ca.crt
update-ca-trust  # don't forget this line
```

And now, running `curl https://gitlab.com` will work.


# Another wrinkle

I thought I had fixed the issue, but the zscaler agent changes its certificate regularly, at least once a week, sometimes more.

So I put the above commands in a script, but I also had to remove the existing trusted root CA. Otherwise the newly added CA wouldn't be considered (somehow?). I'm still not 100% sure why I had to do that though.

```bash
echo "removing existing ca certs for zscaler"
for sig in $(/usr/bin/trust list | grep -B 2 zscaler | grep pkcs11);
do
  echo "removing $sig"
  /usr/bin/trust anchor --remove "$sig"
done
```

And then, create a systemd unit and associated timer to take care of the automatic renewal.

`zscaler.service`
```
[Unit]
Description="Setup zscaler certs"

[Service]
ExecStart=/home/greg/ZscalerMITM/zscaler.sh
```


`zscaler.timer`
```
[Unit]
Description=Run every Monday on boot

[Timer]
OnCalendar=Mon
# triggers even if it misses the last start time
Persistent=true

# register the timer, and make sure it only triggers once network is ready.
[Install]
WantedBy=timers.target multi-user.target
```

Place the service and timer units in the usual location `/etc/systemd/system/` and reload the systemd daemon `systemctl daemon-reload`.

Then enable and start the timer with `systemctl enable zscaler.time && systemctl start zscaler.timer` as usual. No need to do the same for the service.

If the certificate changes midweek, you can manually start the script `systemctl start zscaler.service`.


# Conclusion

With these commands, you can add additional root CA to the OS. Be mindful that this comes with a bunch of security risks, since anyone with the root CA on its hand can inspect your network activity, the padlock icon in the browser's url bar doesn't mean anything anymore.

Also, this will not work for tools which don't rely on the OS certificate authority store. For example, the azure cli uses its own bundled CA roots, so you have to add the zscaler certificates to this bundle.

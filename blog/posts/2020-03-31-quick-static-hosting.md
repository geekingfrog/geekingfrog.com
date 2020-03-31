---
title: Quick static hosting
tags: server, raspberry-pi
status: published
---

The goal was to be able to share files with friends, hosted on my raspberry pi.
Here's the checklist:

* Setup DNS and port forwarding
* Extend [pi-hole](https://pi-hole.net/) lighttpd server
* Setup basic authentication
* Setup letsencrypt


# DNS and port forwarding

Hopefully my ISP doesn't change the IP of my box (or rarely enough). So this is
simply about adding a new `A` record to the result of `curl ifconfig.co`.

Next, connect to the box's management software, usually it's on `192.168.0.1`, or whatever
is the gateway IP for the network. Then enable forwarding port `80` and `443` to the
IP assigned to the raspberry pi.


# Extend lighttpd server

Since pi-hole already has lighttpd bound to the port 80, it needs to be extended with
additional config. In order to avoid clashes with the installer and the existing server,
everything must be placed in `/etc/lighttpd/external.conf`.

Let's put the root of this static hosting under `/home/pi/static_nas/files/`

These files should be somewhat private, so let's put a `robots.txt` at the root:

```
User-agent: *
Disallow: /
```


```conf
# scope the config to the new hostname
$HTTP["host"] == "your-hostname" {

  # don't show anything at the root, only under /vrac/
  dir-listing.activate = "disable"
  $HTTP["url"] =~ "^/vrac/" {
    dir-listing.activate = "enable"
  }
}
```


# Add basic auth

Again, since these files are not meant to be shared to everyone, some auth is required.
Let's go with the simplest thing: [basic auth](https://en.wikipedia.org/wiki/Basic_access_authentication) ([RFC](https://tools.ietf.org/html/rfc7617)). Browsers will prompt for a username and password and transmit that
in an http header. This is _absolutely not secure_ unless tls is used. That's the next step.

```conf
# NEW!
server.modules += (
  "mod_auth"
)

$HTTP["host"] == "your-hostname" {

  server.document-root = "/home/pi/nas/files/"
  dir-listing.activate = "disable"
  $HTTP["url"] =~ "^/vrac/" {
    dir-listing.activate = "enable"

    # NEW !
    # https://redmine.lighttpd.net/projects/lighttpd/wiki/Docs_ModAuth
    auth.backend = "plain"
      auth.backend.plain.userfile = "/home/pi/nas/users.txt"
      auth.require = ( "/" => (
              "method" => "basic",
              "realm" => "though shall not pass",
              "require" => "valid-user"
              )
      )
  } else $HTTP["url"] =~ "^/users.txt" {
    url.access-deny = ( "" )
  }
}
```

And the file for users under `/home/pi/nas/users.txt`. It is placed outside the `document-root` directory
so there is no way to get it through the web server.

```
arandomuser:secretpassword
```

# Add letsencrypt

Basic auth is null and void without tls, so let's set that up. I used [acme-tiny](https://github.com/diafygi/acme-tiny). The README is straightforward.

Let's create a new user and group to interact with letsencrypt.

```
sudo groupadd letsencrypt
sudo mkdir -p /etc/letsencrypt
sudo useradd -g letsencrypt --home-dir /etc/letsencrypt letsencrypt
sudo chown letsencrypt:letsencrypt /etc/letsencrypt
sudo mkdir -p /var/www/challenges
sudo chown letsencrypt:letsencrypt /var/www/challenges
```

The private key for the certificate is `domain.key`, the chained certificate
given by letsencrypt is `signed_chain.crt`, and all of that is placed under
`/etc/letsencrypt/`. The ssl configuration for lighttpd follows:

```conf
# at the top of the file
server.modules += (
  "mod_auth",
  # NEW!
  "mod_alias",
  "mod_openssl"
)

$HTTP["host"] == "your-domain" {
  $SERVER["socket"] == ":80" {
    # the following url is used by letsencrypt to issue challenges to prove
    # the requester owns the domain
    $HTTP["url"] =~ "/.well-known/acme-challenge/" {
      dir-listing.activate = "disable"
      alias.url += ("/.well-known/acme-challenge/" => "/var/www/challenges/")
    } else {
      # Redirect everything else to https
      $HTTP["host"] =~ ".*" {
        url.redirect = (".*" => "https://%0$0")
      }
    }

    $SERVER["socket"] == ":443" {
      server.document-root = "/home/pi/static_nas/files/"
      dir-listing.activate = "disable"

      protocol = "https://"
      ssl.engine = "enable"
      ssl.ca-file = "/etc/letsencrypt/signed_chain.crt"
      ssl.pemfile = "/etc/letsencrypt/signed_chain.crt"
      ssl.privkey = "/etc/letsencrypt/domain.key"
      ssl.disable-client-renegotiation = "enable"
      server.name = "your-hostname"
      setenv.add-environment = (
        "HTTPS" => "on"
      )

      # Got from ssl-config.mozilla.org, disable any legacy versions and ciphers
      ssl.openssl.ssl-conf-cmd = ("Protocol" => "ALL, -SSLv2, -SSLv3, -TLSv1, -TLSv1.1, -TLSv1.2")
      ssl.cipher-list     = ""
      ssl.honor-cipher-order    = "disable"

      $HTTP["url"] =~ "^/vrac/" {
        dir-listing.activate = "enable"
        auth.backend = "plain"
        auth.backend.plain.userfile = "/home/pi/static_nas/users.txt"
        auth.require = ( "/" => (
          "method" => "basic",
          "realm" => "though shall not pass",
          "require" => "valid-user"
          )
        )
      }
    }
  }
}
```

And that's it. I still need to figure out exactly how to reload lighttpd from the cron task when I renew
the certificate. If possible, I'd rather avoid giving root access to this cron.

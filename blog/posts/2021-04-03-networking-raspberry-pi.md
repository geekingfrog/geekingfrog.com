---
title: Networking and raspberry pi
tags: server, networking
status: published
---

# Network configuration for the raspberry pi

My sd card got corrupted so I had to re-install the OS. I used the script [create_ap](https://github.com/oblique/create_ap) to use my raspberry pi as a WIFI access point, sharing its wired connection. However I discovered that it is no longer maintained. So I had to (finally) dive into some linux networking basics.


My raspberry pi is completely headless, I don't have a screen or a keyboard connected to it. It has an IP address from its wired connection, and I can ssh onto it, but when I screw up the network config, I'll be stranded.

# Wifi setup

Before using the wireless card as an access point, let's first enable it to get network access from it. This way, I can muck around `eth0` and the bridge until it works, without having to transfer and mount the sdcard on another machine.

First, unblock the network card `rfkill unblock wlan`.

Then, create the wifi configuration. This one is using WPA2.
`wpa_passphrase SSID PASSWORD > /etc/wpa_supplicant/wpa_supplicant-INTERFACE.conf`

Once this file is in place, rename the existing file `/lib/systemd/system/wpa_supplicant@.service` by adding the interface name after the `@`. In my case it's `wpa_supplicant@wlan0.service`. Starting this service will use the config file created previously and connect to the network. You can check that with `ip a`, the line for `wlan0` should show a line with an ip address.

Then, I can ssh into the pi using the wifi address.


# Bridge setup
Now that I have a reliable connection to the pi, I can wreck `eth0`.

Create the bridge interface by creating the following file under `/etc/systemd/network/br0.netdev`
```
[NetDev]
Name=br0
Kind=bridge
```

To check if that worked, restart `systemd-networkd.service`, and then `ip a` should display an entry for `br0`.

Then, bind `eth0` to the bridge with `/etc/systemd/network/br0-members.network`
```
[Match]
Name=eth0

[Network]
Bridge=br0
```

Finally, create a bridge network with the file `/etc/systemd/network/br0.network`:

```
[Match]
Name=br0

[Network]
DHCP=ipv4
```

Enable all of that: `systemd enable systemd-networkd.service`, and configure `dhcpcd` to give an IP address to `br0` but not to `eth0` by adding the following lines to `/etc/dhcpcd.conf`:

```
denyinterfaces eth0
interface br0
```

With all of that done, `ip a` should show no ip address for `eth0`, and the text `master br0`, meaning this interface is now under the control of `br0`. The line for `br0` should show the ip address instead.


# Access point setup

Now that the ethernet bridge works fine, let's disable the wifi and use `hostapd` to create a software access point. `systemctl stop wpa_supplicant@wlan0.service && systemctl disable wpa_supplicant@wlan0.service`. Let's also configure `dhcpcd` to prevent getting dhcp leases on the wireless interface. Simply modify the line in `/etc/dhcpcd.conf`: `denyinterfaces eth0 wlan0`.

Grab this [default config file](https://wiki.archlinux.org/index.php/Software_access_point#Wi-Fi_link_layer) and edit the important bits: the `interface`, the `bridge`, the `ssid` and the `wpa_passphrase`. Also, don't forget to change the `country_code` to configure the access point with the right frequencies. In my case it's `GB`. No need to quote the passphrase if there are multiple words. There is a minimum length of 8 char for it. Passphrase under that limit will make `hostapd` fails with a cryptic error message.

A first simple check is to manually run `hostapd` with this configuration and check that another device can connect to it and access the internet: `hostapd -S /etc/hostapd/br0.conf`.

When that's working, rename the systemd service and enable it:
```
mv /lib/systemd/system/hostapd@.service /lib/systemd/system/hostapd@br0.service
sudo systemctl start hostapd@br0.service
sudo systemctl enable hostapd@br0.service
```

And tadaaaa, everything is now working. Next, install [pi-hole](https://github.com/pi-hole/pi-hole/), but that's straightforward.


# Resources

* Wireless access point for raspberry pi os documentation: [https://www.raspberrypi.org/documentation/configuration/wireless/access-point-bridged.md](https://www.raspberrypi.org/documentation/configuration/wireless/access-point-bridged.md)
* Network bridge configuration on (arch) linux [https://wiki.archlinux.org/index.php/Network_bridge#With_systemd-networkd](https://wiki.archlinux.org/index.php/Network_bridge#With_systemd-networkd)
* Systemd networkd bridge interface: [https://wiki.archlinux.org/index.php/Systemd-networkd#Bridge_interface](https://wiki.archlinux.org/index.php/Systemd-networkd#Bridge_interface)

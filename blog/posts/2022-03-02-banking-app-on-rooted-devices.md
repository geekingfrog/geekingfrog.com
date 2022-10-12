---
title: Notes on ssh agent
status: draft
tags:
- ssh
- server
---

# Installing Magisk

* `adb install` the apk.
* Download [latest lineageOS](https://download.lineageos.org/) rom for your device.
* Unpack the `.zip` file, there should be a `payload.bin` file inside.
* Use `payload_dumper` to extract `boot.img` from `payload.bin`
* `adb push boot.img /sdcard/Download/` to put the boot image on the device.
* Launch Magisk on the device and then click Install at the very top. Select the `boot.img` and let magisk patch it.
* Pull the patched image back `adb pull /sdcard/Download/magisk_patched-XXXXXX.img ./`
* Reboot the device in fastboot mode: `adb reboot fastboot`.
* The device should have a black screen, with a blue led. Confirm it's ready by typing `fastboot devices`. It should show some device attached.
* Flash the patched boot image `fastboot flash boot magisk_patched-XXXXXX.img` and then reboot.

And there you go, Magisk is now installed on the device.


# ???

This section basically follows [this youtube tutorial](https://www.youtube.com/watch?v=VCTYp09Dc74).

## Disable hardware key attestation

If the device uses hardware backed key attestation, we need to trick it into thinking it's not available.

* Download [MagiskHideProps](https://forum.xda-developers.com/t/module-magiskhide-props-config-safetynet-prop-edits-and-more-v6-1-2.3789228/) module and put it somewhere on the device.
* Launch magisk
* At the bottom right, click on the `module` link, and install the module from disk. Reboot the device.
* With the module installed, launch a terminal (adb or [termux](https://termux.com/))
* Type `sudo -c props`, grants termux superuser rights for this to work.
* Select `2` for basic key attestation.
* For my device, the default value is `D6503`, and the currently loaded value is `H3113`.



# Sources

https://www.xda-developers.com/how-to-install-magisk/
* [payload dumper](https://github.com/vm03/payload_dumper)
* Youtube tutorial to use [banking apps on rooted devices](https://www.youtube.com/watch?v=VCTYp09Dc74).
* [MagiskHideProps](https://github.com/Magisk-Modules-Repo/MagiskHidePropsConf/blob/master/README.md) github repo.

---
title: Cyanogenmod and Htc
tags: android, cyanogenmod, geek
status: published
---

After 5 years of good service, my phone was really too slow and I decided to change it.  Even with cyanogenmod which gave it a second life, doing anything would require seconds.  So with the new device (htc one a9), I also wanted to have cyanogenmod, an awesome custom rom. I thought the whole process would take me one or two hours, but I actually wasted 2 full days.  I believe I messed up everything I could, and almost bricked the phone twice.  Below is some sort of checklist for troubleshooting, should the need arise again in the future.

# The right way
I could have saved myself countless hours with this recommendation: **apply the OTA updates FIRST**. The OTA updates expect a stock rom, stock recovery and locked bootloader. Once I started tinkering with the phone, they would not install properly.

Also, the cyanogenmod version available for my device requires a fairly recent kernel, and would refuse to be installed on older version. The original kernel was too old for cyanogenmod unfortunately.

# Bootloader manipulations

## Unlock htc bootloader
With htc, unlocking the bootloader is more complex than with my previous nexus device, but the whole
process [is documented](http://www.htcdev.com/bootloader/) and very straightforward. Do **not** forget to enable **unlock bootloader** in the developper settings from the system before anything !

When flashing the `Unlock_code.bin` file, the phone should be in the `download` mode (adb reboot download, or select it from the bootloader).

## Relock the bootloader
To reflash the stock rom and recovery, the bootloader must be locked. To do it, put the phone in download mode and use the command `fastboot oem lock`.

## S-OFF
To completely unlock the bootloader, an optional step is to modify the `S-ON` flag to `S-OFF`. The way to do it is to install [Sunshine](http://theroot.ninja/) as a regular application. `adb install sunshine-latest.apk`. Afterwards, follow the instructions on the screen. It costs $25, but it's worth it if you want full control on your phone. The process will wipe out any data you have on the phone, so do it on a fresh install. It will also **not** work on a cyanogen rom, so I recommend doing it on the stock rom, once all the OTA updates have been applied, just before installing TWRP and cyanogenmod.


# Restore stock system and recovery

## Get device model ID and CID
Start the device in `download` mode. The CID and MID should be visible at the top. For example, my htc One a9 has:

```
CID-HTC__621
MID-2PQ910000
```

## Get RUU installer and correct rom.zip files
For the htc a9, a full list of stock rom archive can be found [on the xda forum](http://forum.xda-developers.com/one-a9/general/wip-ruu-htc-one-a9-t3240344). Grab the latest one for your device, as identified by the `MID` and `CID`. This archive is **not** a regular zip archive. If you try to flash it directly you'll encounter this error:

```bash
$ fastboot oem rebootRUU
$ fastboot flash zip rom.zip

[...]
invalid sparse file format at header magi
[...]
```

The workaround is to use htc's executable to update the rom. I couldn't find the correct executable for my device so I downloaded a random one for an htc a9 [on the htc us site](http://www.htc.com/us/support/rom-downloads.html). (It's windows only unfortunately).

* Relock the bootloader
* Put the device in `download` mode
* Start the `.exe` and stop at the screen with the `open README` button
* Go to `Users/xxx/AppData/Local/Temp/` There should be a folder named with a UUID, with a subfolder with the same name. Inside, there is a file named `rom.zip`. That's the one you should replace with the correct RUU archive previously downloaded. See [this thread to locate the rom.zip from the ruu installer](http://forum.xda-developers.com/showthread.php?t=2534428).
* Once the `rom.zip` file has been replaced, proceed with the installer. It will take a few minutes (~10 min in my case) and reboot a few times.

Tadaa! The phone is now fresh with a stock rom.


# Install cyanogenmod and the openGapps

## Encrypted partition
Upon booting with TWRP recovery, it will complain the `data` partition cannot be mounted because it's encrypted. Just press `cancel`, swipe to allow modification and **format** the `data` partition. This will of course erase everything in this partition.

## Blacklisted SBL1 when installing cyanogenmod
This is because the device's kernel is not supported by cyanogenmod (too old). To avoid that, install the official OTA, which will upgrade the kernel, and then cyanogenmod can be updated.

## Error loop at boot time
When installing the openGapps, I got `update Google Play Services error` and `Setup Wizard has stopped working` in a loop. This is due to missing permission to the Setup Wizard app. To avoid that, flash cyanogenmod, and right after, flash the gapp, **without leaving the recovery mode**. Only after **both** are flashed, the device can be rebooted.


# Conclusion

This whole mess took me a full 2 days to sort out. The xda forums are very good, but it's too often like recipe without exlanation of what's wrong and why the recommended steps work. So when it doesn't work (often), you're back at square one. This is a very frustrating and time consumming process.

I also learned that it's **very** hard to completely brick a phone. Unless you somehow delete or corrupt the bootloader, you can always recover.

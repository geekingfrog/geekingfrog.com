---
title: Full GPG setup
tags: 
status: draft
---

If you care about security and encryption, gpg is very good. It's used by git and the amazon command line sdk whenever something needs to be signed. Pgp uses a system of public/private key, the same used for TLS/SSL.

## Quick intro about GPG
Using gpg encryption, you can guarantee the authenticity and the identity of the author of a document. How does that work? You create two keys, a private key used for signing and decrypting document and a public key to encrypt document. As the name suggest, the public key is known for everyone who wants it. Using my public key, anyone can encrypt a document, and the result can only be decrypted by me, with my private key.

One important concept of GPG is the *web of trust*. If you trust me, you can sign my public key with you private key. It means that you trust that this public key effectively belongs to me, and you add your personal signature on it. It works like a recommendation letter with your signature at the bottom.

GPG also allow for revocation of keys. Usually, one does not directly use it's private key to sign document but uses a subkey. In this scenario, if the subkey becomes compromised (someone guessed your passphrase for example), it can be marked as revoked, and a new one can be generated. This process preserve all the trust given by others. Also the master key should be kept very secured (on a separate flashdrive for example), it is often impractical to use it all the time. That's one more reason to use subkeys.
If the master key become comprosided, there is no way but to start the chain of trust from scratch.

Below I'll detail a full setup to:

* Create a master key with an expiration date
* Create a subkey with a shorter expiration date.
* Create a revocation certificate for the master key and the subkey.
* Upload the public key to a keyserver
* Extract the master key to a separate device

A lot of this post is coming from [pgp best practices](https://help.riseup.net/en/security/message-security/openpgp/best-practices).

## Improving default config
To make sure gpg is not using unsafe algorithms and settings, add the following snippet to `~/.gnupg/gpg.conf`

```
#-----------------------------
# algorithm and ciphers
#-----------------------------

# list of personal digest preferences. When multiple digests are supported by
# all recipients, choose the strongest one
personal-cipher-preferences AES256 AES192 AES CAST5

# list of personal digest preferences. When multiple ciphers are supported by
# all recipients, choose the strongest one
personal-digest-preferences SHA512 SHA384 SHA256 SHA224

# message digest algorithm used when signing a key
cert-digest-algo SHA512

# This preference list is used for new keys and becomes the default for
# "setpref" in the edit menu
default-preference-list SHA512 SHA384 SHA256 SHA224 AES256 AES192 AES CAST5 ZLIB BZIP2 ZIP Uncompressed

```

## Create a public/private key pair

`gpg2 --full-gen-key`

```
Please select what kind of key you want:
   (1) RSA and RSA (default)
   (2) DSA and Elgamal
   (3) DSA (sign only)
   (4) RSA (sign only)
Your selection?
> 1
```

```
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (2048)
> 4096
```

```
Key is valid for? (0) 2y
```

Then, provide your real name and the email address. The comment is not necessary and might be confusing, I'll avoid it. You'll be asked for a passphrase, choose [a long one](http://imgs.xkcd.com/comics/password_strength.png) and you should be good. This may take some times for the system to generate enough entropy (random bytes). To speed up the process, you can start some `find` or `grep` on the whole filesystem and play some silly games.
Note that **it is possible to extend the expiration date of the key** even after the key has expired.

When the process is complete, you can check you have your key with `gpg --list-keys` and `gpg --list-secret-keys`.

```
gpg --list-keys
/home/greg/.gnupg/pubring.gpg
-----------------------------
pub   4096R/90805933 2014-02-09 [expires: 2019-02-08]
uid                  Grégoire Charvet <greg@geekingfrog.com>
sub   4096R/DCE77214 2014-02-09 [expires: 2019-02-08]
```

## Create a subkey
To create a key for signin, grab you key id and edit it:

```
gpg --list-keys
gpg --edit-key <keyid>
```

You'll enter an interactive prompt:

```
gpg> addkey
```
Type your passphrase and chose the `RSA (sign only)` option. Don't forget to type `save` after the key have been created.

You can also repeat the process to create a subkey for encryption.

## Generating revocation certificate

```
gpg --output revokeMaster.asc --gen-revoke <keyid>
```

Store this revocation certificate away from your private key. If the key becomes compromised, you can publish this certificate to revoke it.

## Remove the private key from your hard drive

## Last tips
If you want to know what subkey is capable of doing what, you have to  `gpg --edit-key <key-id>` and you'll have the list of keys with the `usage` field. `S` means signing, `C` means certifying (not sure what it is) and `E` means encrypting.

---
Sources:

* [subkey doc](https://wiki.debian.org/subkeys)
* [Open PGP best practices](https://we.riseup.net/riseuplabs+paow/openpgp-best-practices)
* [tutorial and overview](http://zacharyvoase.com/2009/08/20/openpgp/)

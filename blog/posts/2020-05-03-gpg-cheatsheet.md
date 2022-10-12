---
title: GPG usage cheatsheet
status: published
tags:
- gpg
---

Braindump of some common command.

## Import a public key
`gpg --import path-to-public-key`
To change the trust level given to that key:
`gpg --edit-key <key-id>` then `trust`.

To verify everything is fine: `gpg --list-keys`

## Decrypt a message using your private key
`gpg --decrypt --output output-file encrypted-file.asc`

## Encrypt a message using the public key of the recipient
`gpg --armor --encrypt --recipient <key-id> --output encrypted.asc plaintext-source`

`--armor` will generate an ASCII file instead of a binary file.

## Sign a message
In gpg parlance, to sign a message means encrypt it with the private key. Anyone with the public key can decrypt it.

`gpg --armor --sign --local-user <key-id> plaintext-source`

If you only want to prove integrity without encrypting the cleartext:
`gpg --armor --clearsign --local-user <key-id> plaintext-source`

## Symmetrically encrypting something
`gpg --symmetric --output ciphertext-symmetric plaintext`
and then
`gpg --decrypt --output decrypted ciphertext-symmetric`

# More
See [my full gpg setup](./full-gpg-setup) for setting up the whole thing.

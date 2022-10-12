---
title: Using podman instead of docker
status: draft
tags:
- linux
- docker
- podman
---

# Podman?

With the [recent announcement](https://www.docker.com/blog/updating-product-subscriptions/) from docker that docker desktop is not going to be free anymore for enterprise, and the [timely announcement](https://podman.io/blogs/2021/09/06/podman-on-macs.html) that [podman](https://podman.io/) is available on mac, I decided to dig a bit.

I had no idea what podman was. I actually use linux pretty much full time, so I wasn't bothered by the change of license for docker desktop. However, what was really interesting for me was the lack of a daemon for podman, and the ability to run [rootless container](https://github.com/containers/podman/blob/main/docs/tutorials/rootless_tutorial.md). Now, that is worth switching over.


# Config

This is mostly a quick list of things to do next time I have to redo the setup. It's mostly about following the [rootless tutorial](https://github.com/containers/podman/blob/main/docs/tutorials/rootless_tutorial.md).

* On archlinux, `pacman -S podman` should be enough to also pull the required dependencies.

* Setup subuid and subgid. `sudo touch /etc/subgid /etc/subuid` and then `sudo usermod --add-subuids 200000-201000 --add-subgids 200000-201000 $(whoami)`

* Configure podman to use dockerhub (the default with docker). In the file `$XDG_CONFIG_HOME/containers/registries.conf`, put `unqualified-search-registries = ["registry.hub.docker.com"]`.

* Configure where to store rootless images in `$XDG_CONFIG_HOME/containers/storage.conf`:

```toml
[storage]

rootless_storage_path = "$HOME/.local/share/containers/storage"
driver = "overlay"
```

To test that everything is working as expected, you can run a quick smoke test: `podman run --rm -it wernight/funbox nyancat`.

* And then, you can alias the command: `alias docker=podman`.

# Bonus: AWS ECR auth

If your corporate overlord is using aws with ECR, you'll need to authenticate with it.

```
aws ecr get-login-password | podman login \
  --username AWS \
  --password-stdin \
  <ACCOUNT_ID>.dkr.ecr.<REGION>.amazonaws.com
```

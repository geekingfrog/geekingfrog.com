---
title: Elixir clusters in kubernetes
status: published
tags:
- elixir
- k8s
---

This post is mostly a bunch of notes for my future self if I ever want to touch this again.
A rough journal of the main steps to setup a local kubernetes cluster, that
can route external traffic to internal apps, and having a sample phoenix app
that can automatically form clusters with other pods.


# Talos

[Talos](https://www.talos.dev/) linux is an immutable linux distribution meant
to run kubernetes nodes. It is really minimal and only ship with the bare minimum
to run k8s. The only way to modify the nodes is through API call applying
configuration from files, in a rather declarative way.

I am using [incus](https://linuxcontainers.org/incus/) to manage the vms for
the nodes, with [special talos images](https://github.com/siderolabs/talos/discussions/10157#discussioncomment-11870199) made for incus.

Following the tutorial for talos works just fine. Perhaps the one thing to be
aware of is that the default memory for the VM are too low and must be increased.

```shell
incus config set talos-minion-1 limits.memory=2300MiB
```

this may need to stop the vm to make this happen.

# Routing and networking

I didn't bother seting up a firewall on the talos nodes: any incoming or outgoing
traffic is allowed. Since everything is running locally, I don't care about that.

Then I need a [gateway](https://kubespec.dev/gateway-api), I went somewhat randomly with
[traefik](https://doc.traefik.io/traefik/getting-started/kubernetes/).

This requires to install some custom resources definitions (crd):
```shell
helm install traefik-crds traefik/traefik-crds
```
This may only be required if using `TCPRoute`, so maybe not actually useful.

The traefik guide works just fine, look out for stuff like `insecure: true` to
avoid any TLS setup.

Muck around with `/etc/hosts` to target the control plane node with some hostname
so you can define some routing rules with `HTTPRoute` based on hostname.

Note that the trafic goes through the **control plane node** and not the work nodes.
And so, any hostname setup should us the control plane IP.

# Podman and containers

When building images locally, they also need to be published somewhere. I ran
a local registry with:

```shell
podman run --rm -v registry-volume:/var/lib/registry -p 5000:5000 --name local-registry registry:2
```

From within an incus node, the gateway IP route to the host network (I have a bridge managed by incus).

In my case, the image location for k8s are: `image: 10.134.83.1:5000/xxx`


# Elixir and libcluster

## Elixir release

The erlang release has to be setup just right such that each node has a very specific name
that works out the box with [libcluster](https://hexdocs.pm/libcluster/readme.html) and is also
routable within the cluster.

in `env.sh.eex`, to setup the node name and the port for the erlang distribution

```
export POD_A_RECORD=$(echo $POD_IP | sed 's/\./-/g')
export CLUSTER_DOMAIN=cluster.local
export RELEASE_DISTRIBUTION=name
export RELEASE_NODE=<%= @release.name %>@${POD_A_RECORD}.${NAMESPACE}.pod.${CLUSTER_DOMAIN}


case $RELEASE_COMMAND in
  start*|daemon*)
    ELIXIR_ERL_OPTIONS="-kernel inet_dist_listen_min $BEAM_PORT inet_dist_listen_max $BEAM_PORT"
    export ELIXIR_ERL_OPTIONS
    ;;
  *)
    ;;
esac
```

This will generate node with names like `live_node@10-244-0-92.default.pod.cluster.local` (the app is called live_node).

The libcluster config I used:
```elixir
config :libcluster,
  topologies: [
    k8s: [
      strategy: Cluster.Strategy.Kubernetes,
      config: [
        kubernetes_node_basename: "live_node",
        kubernetes_selector: "app=live_node",
        kubernetes_namespace: "default",
        kubernetes_ip_lookup_mode: :pods,
        mode: :dns
      ]
    ]
  ]
```

This requires a service account with the following role:

```yaml
---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: libcluster-role
  namespace: default
rules:
  - apiGroups:
        - ""
    resources:
      - endpoints
      - pods
      - services
    verbs: ["get", "list", "watch"]
```

also need to create the `ServiceAccount` and `RoleBinding` resources, RTFM.

## K8S config

The erlang cookie should also be the same for all pods, this can be set through env vars in a configMap:

```
PHX_HOST: "erl.local"  # this is just for routing in my case
BEAM_PORT: "9000"
RELEASE_COOKIE: "SPMCSUDPWPPRVSKNNYLU" # ideally this would be from a k8s secret resource
```

and some additional dynamic env vars to be set in the deployment definition:

```
env:
  - name: NAMESPACE
    valueFrom:
      fieldRef:
        fieldPath: metadata.namespace
  - name: POD_IP
    valueFrom:
      fieldRef:
        fieldPath: status.podIP
  - name: POD_NAME
    valueFrom:
      fieldRef:
        fieldPath: metadata.name
```

## Remote into a pod

* make sure there is no local epmd: `epmd -kill`

* port forward for epmd and erlang distribution to a pod:
`k port-forward live-node-db6dc56cd-l7qt6 4369 9000`

* make sure the name of the remote node is bound to localhost (muck around with `/etc/hosts`).

* start a local iex: `iex --name local@example.com`

```elixir
:net_adm.names # check what's know to epmd
Node.connect(:"live_node@10-244-0-102.default.pod.cluster.local")
```



---
title: Run ssh commands on multiple ec2 instances
tags: aws, geek
status: draft
---

One of my recent task was to restart a service after a database migration. The "problem" was
that this service was deployed using aws autoscaling group, and so there was close to 10
instances. This post is about how to automate this.

# Grab all private IPs for a given autoscaling group
First, the [aws cli](http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html) can be
used to get a big json blog describing all running ec2 instances.

```bash
aws ec2 describe-instances help
```

There is a way to narrow down the result list using filters. The list is huge, and one looked promising: image-id.
All instances in an autoscaling group use the same AMI (amazon vm images), so that's easy to use.

```bash
aws ec2 describe-instances --filters Name=image-id,Values=ami-xxxx
```

The result is a *big* json blob, and I'm only interested in the private IPs of the instances. To get
these easily, I used [jq](https://stedolan.github.io/jq/), a command-line JSON processor. The syntax
is a bit weird at first, but it's a very powerful and convenient tool, always handy to have.

```bash
jq ".Reservations[].Instances[0].PrivateIpAddress"
```

This retrieve all items in the toplevel `Reservations` object (it's an array). Then, It get the first
`Instances` item, and focus on the `PrivateIpAddress`

The final command is:

```bash
aws ec2 describe-instances --filters Name=image-id,Values=ami-xxxx \
| jq ".Reservations[].Instances[0].PrivateIpAddress" > ips
```

## Execute a command for all instances
Good old bash is perfect for this job:

```bash
for ip in $(cat ips); do ssh username@${ip:1:-1} 'ls -al'; done
```

Note the use of substrings. The ips retrieved in the previous section are quoted, so we need to remove them with the syntax `${ip:1:-1}` which acts the same as python ranges: `my_string[1:-1]`

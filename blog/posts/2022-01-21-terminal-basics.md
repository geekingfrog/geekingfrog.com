---
title: Terminal basics
status: published
tags:
- linux
- shell
---

# Unixy terminal basics

This article is an introduction to the Command Line Interface (`cli`) in general on unixy machines, like macOS or linux. Target audience is (complete) beginner about the shell, but not programming.

Perhaps your only exposure to the shell so far was through instructions to install something, and you blindly copy pasted some instructions like `brew install fzf` or `pacman -Syu`. And now, you want to level up, or through various circumstances, you now need to become a bit better at using command line interfaces.

This post aims to describe some very basics, some general conventions, and how to find out more when stuck.

# The shell and the terminal emulator

A shell is an interactive program that allow the user to start and manage other programs. The terminal emulator is the software providing the graphical interface (GUI), handling input and displaying output. They are called so because they are reproducing the interface and features of ancient terminals:

![pdp terminal](https://upload.wikimedia.org/wikipedia/commons/thumb/8/87/Televideo925Terminal.jpg/330px-Televideo925Terminal.jpg)

These were the interface connected to the actual computer, a massive machine safely store in a dedicated room.

On macOS, a popular terminal is [iterm2](https://iterm2.com/). On linux, there are many terminal emulators, some popular ones are [urxvt](https://wiki.archlinux.org/title/Rxvt-unicode), [alacritty](https://github.com/alacritty/alacritty) or [kitty](https://sw.kovidgoyal.net/kitty/). I'm less familiar how things work on Windows, [PuTTY](https://en.wikipedia.org/wiki/PuTTY) was mentionned as relatively standard there.

The shell is the default program run inside the terminal emulator.

When opening a terminal, you're faced with a prompt, where you can type commands. Something like
```
$ █
```

Where `█` indicates your cursor. This is the prompt of your shell, and this is where you can type any command. To know what shell you are using, you can type the following command:

```
$ echo $SHELL
/bin/zsh
```

The line `/bin/zsh` is the result of executing the command `echo $SHELL`. This means the shell I'm currently running is the [z shell](https://en.wikipedia.org/wiki/Z_shell). Other popular shells are [bash](https://en.wikipedia.org/wiki/Bash_%28Unix_shell%29) and [fish](https://fishshell.com/).

If you have no idea which one to choose, I'd recommend the z shell, as it has decent defaults and good compatibility. The difference between shells is around their own language for scripting, some utilities for completion, and how they run and execute commands. For this article, there shouldn't be any difference.


# Navigating and editting the command line


Now, the first step is to be comfortable jumping around. For iterm2, [this article](https://heyshrey.com/article/configure-iterm) explains how to configure it so that you can quickly move the cursor. For linux, these things usually work out of the box.

```
$ Lorem ipsum dolor sit amet█
# pressing ctrl+← or ⌥+← on mac makes the cursor jump one word backward
$ Lorem ipsum dolor sit █met
# pressing that again moves yet another word backward
$ Lorem ipsum dolor █it amet
# and then, ctrl+→ or ⌥+→ moves in the other direction:
$ Lorem ipsum dolor sit █met
# pressing ctrl+w on linux, or ⌥+<backspace> deletes the previous word
$ Lorem ipsum dolor █met
# you can also jump at the beginning of the line with ctrl+a
$ █orem ipsum dolor amet
# and at the end of the line with ctrl+e
$ Lorem ipsum dolor amet█
```

Also, pressing ↑ (arrow up) will replace your current line with the previous command you executed. Pressing ↓ (arrow down) will go the opposite direction.

```
$ ls /
bin   dev  home  lib64       mnt  proc  run   srv  tmp  var
boot  etc  lib   lost+found  opt  root  sbin  sys  usr

$ █
# pressing ↑ will show the previous command
$ ls /
```

# Commands options and arguments

Some commands work directly on their own. For example:

## Arguments

```
$ date
Wed Oct 13 02:17:48 PM BST 2021
```

Many commands can take additional arguments and options. An argument is an additional word after the command. For example:
```
$ ls / /tmp
```
There, the command is `ls`, and it is given two arguments: `/` and `/tmp`.

## Options (and flags)

An option is anything starting with a single or double dash: `-` or `--`. The convention is that a single dash is followed by single letter options, and double dashes are used for longer options. When the option doesn't expect any argument, it's called a (boolean) flag.

```
$ ls -l --almost-all --directory
```
This command has three options: `-l`, `--almost-all` and `--directory`.

It is also usually possible to merge multiple shorthand options together:

```
$ ls -al
# same as
$ ls -a -l
```

Some options accept arguments. All of the following forms are equivalent:

```
$ ls --width 3
$ ls --width=3
$ ls -w 3
$ ls -w3
```


## Repeating flags

Some flags can be repeated. A usual one is `-v` to increase logging verbosity:

```
ssh      greg@geekingfrog.com # no logging
ssh -v   greg@geekingfrog.com # info logs
ssh -vv  greg@geekingfrog.com # more verbose logs
ssh -vvv greg@geekingfrog.com # extra super verbose logs
```

## Interactive mode

Some commands have an interactive mode, where they ask the user to confirm some actions before proceeding. For example:

```
$ Continue [Y/n]?█
```
You are then supposed to enter yes or no. In this case, usually `y`, `Y`, `n`, `N` work. The convention is to have the uppercase letter be the default option. So simply typing `enter` without anything will default to yes (`Y`).


# Getting help

There are three main ways to figure out what to do.

## Asking the command itself for some help

Pretty much every single command will accept `-h` and `--help` to display a help message, then exit. Whever you're faced with a new command, try these and see what happens.

## Discovering things with autocompletion

Whenever you are typing a command, you can autocomplete it by pressing `<tab>`. The completion behaviour will depend on your shell. For example:

```
$ ls<tab>
LSCOLORS   lsa        lsbom      lsm        lsof
ls         lsappinfo  lskq       lsmp       lsvfs
```
I haven't pressed `<enter>` anywhere there, just pressed `<tab>` when my cursor was right after `ls` (no space). What's displayed below is the list of commands starting with ls. Pressing `<tab>` multiple time allow me to cycle through the available commands. For example, after pressing `<tab>` three times, it looks like that.

```
$ lsa
LSCOLORS   lsa        lsbom      lsm        lsof
ls         lsappinfo  lskq       lsmp       lsvfs
```

Whenever you're in doubt, try pressing `<tab>` and see what happens. Some shells like `zsh` or `fish` have really powerfull completions.


## Using manual pages

Commands also come with a user manual. This is invoked with the command
```
$ man ls
```

The manual will be displayed through a command called a pager. By default, it's often a command called `less`. Here are a couple of useful things to know about `less`:

* To exit it: press `q`
* `pageDown` and `pageUp` for navigation. If your keyboard don't have these keys, `ctrl-f` and `ctrl-b` (standing for forward and backward) have the same effect.
* Pressing `/` will allow you to enter a search term. Then, pressing `n` will jump to the next occurence, and `N` for the previous match. There is no wrapping for search, so no match may just mean all the matches are before your cursor position.
* Pressing `g` jumps at the beggining of the document, while `G` jumps at the end.



# Conclusion

Now you should have some basics to interact with command line tools, and more importantly, the means to discover more if you're stuck.
Perhaps the best advice I can give is to notice what you're doing a lot, either consciously or by recording your screen and reviewing it later. And then, see if you can improve anything that seems repetitive or disruptive.


---

Thanks to Alex for their proofreading and feedback.

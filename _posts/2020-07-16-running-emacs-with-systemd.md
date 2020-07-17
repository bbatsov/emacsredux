---
layout: post
title: Running Emacs with systemd
date: 2020-07-16 12:30 +0300
tags:
- Linux
- Daemon
---

Recently I've switched back to Linux, after having used macOS for the past 9 years.
While I was generally happy with my overall macOS experience, I was also disappointed
that Emacs simply didn't work as well there, as it does on Linux for various reasons.

When I was a Linux user I'd always run Emacs as a daemon (server) and I'd
connect this daemon from multiple instances of `emacsclient`. This was both
elegant and efficient - my clients started instantly and shared access to
everything that was running on the daemon instance. While this was doable to
some extent in macOS, it never worked quite as well for me, and I abandoned that
workflow eventually. Now, however, I'm back! Time to revive the workflow!

Historically I ran the Emacs daemon by adding something like this to my shell init (e.g. `.bashrc`):

``` shell
export ALTERNATE_EDITOR=''
alias e='emacsclient --tty'
```

The magic is in the first line - leaving `ALTERNATE_EDITOR` blank.
That way the first time I ran `emacsclient` it'd start an Emacs daemon and
connect to it. Many people preferred to make the daemon a "proper" service that
they can start, restart and monitor, but this felt like an overkill to me. I've
noticed, however, that Emacs 26.1 bundles a `systemd` unit, so it's now trivial
to control your Emacs daemon with `systemd`. It all boils down to running this command:

``` shellsession
$ systemctl --user enable --now emacs
```

Run this command with your regular user (or whatever user account you want to be running Emacs). Don't run it as `root`, though!
You'll get a message that the unit file was copied to `/usr/lib/systemd/user/emacs.service` and you can examine it if you're curious:

```
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=simple
ExecStart=/usr/bin/emacs --fg-daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
```

Pretty straightforward.

At this point your Emacs daemon is up and you can connect to it using both
terminal clients (`emacsclient -t`) and GUI clients (`emacsclient -c`). You
might also want to create some desktop icon that runs `emacsclient -c`, instead
of `emacs`.  You might also want to set both `EDITOR` and `VISUAL` to
`emacsclient -t`:

``` shell
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -t'
```

I typically add two more aliases just to be on the safe side:[^1]

``` shell
alias vi='emacsclient -t'
alias vim='emacsclient -t'
```

One thing to keep in mind is that when you're running Emacs in this manner it
won't read your user environment variables (at least not those coming from your
`.bash_profile` and `.bashrc`). That's why it's a good idea to install the
popular package
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

Alternatively you can use one of `systemd`'s own mechanisms for setting environment variables - e.g. [environment.d](https://www.freedesktop.org/software/systemd/man/environment.d.html).
The [Arch Wiki](https://wiki.archlinux.org) has
a few [good examples](https://wiki.archlinux.org/index.php/Systemd/User#Environment_variables) of
using `environment.d` and also mentions other approaches that you can consider.

That's all I have for you today. I hope you've learned something useful. In parentheses we trust!

[^1]: Old habits die hard.

---
layout: post
title: Remap Enter to Control in GNU/Linux (2020 Edition)
date: 2020-07-05 15:04 +0300
comments: true
tags:
- Linux
- Keybindings
---

**Note:** Check out my [original article from 2013]({% post_url 2013-11-12-a-crazy-productivity-boost-remap-return-to-control %}) about the rationale behind this remapping.

Recently I've switched back from macOS to GNU/Linux, as my primary development
environment, and I found out that my [old article]({% post_url 2016-01-30-remap-return-to-control-in-gnu-slash-linux %}) on remapping `Enter` to
`Control` was no longer the optimal way to achieve this. It took me a bit of
digging, but eventually I found
[dual-function-keys](https://gitlab.com/interception/linux/plugins/dual-function-keys) (a plugin
for the [interception framework](https://gitlab.com/interception/linux/tools)),
which does exactly what I needed and it does it splendidly.

Unfortunately, the tool is not packaged for most
GNU/Linux distros[^1], but setting it up from source is not that complex. In this article
I'll share instructions that are specific to Ubuntu, but they should be
easy to modify for other Linux distros.

Let's kick it off by downloading and installing the `interception` framework and
`dual-function-keys`:

``` shellsession
# install build deps
$ sudo apt install libudev-dev libyaml-cpp-dev libevdev-dev cmake
# create a folder where to clone the source code
$ mkdir src && cd src
# clone the necessary code
$ git clone https://gitlab.com/interception/linux/tools
$ git clone https://gitlab.com/interception/linux/plugins/dual-function-keys
# build and install the interception framework
$ mkdir build
$ cd build
$ cmake ..
$ make
$ sudo make install
$ cd ../..
# build the dual-function-keys plugin
$ cd dual-functions-keys
$ make && sudo make install
```

That wasn't so hard, right? Now we have to create a couple of configuration files and we're ready for action. The first one is `.dual-function-keys.yaml`:

``` yaml
TIMING:
  TAP_MILLISEC: 200
  DOUBLE_TAP_MILLISEC: 150

MAPPINGS:
  - KEY: KEY_ENTER
    TAP: KEY_ENTER
    HOLD: KEY_RIGHTCTRL
```

That's the main config for `dual-function-keys`, where we're specifying the duration of a tap and double tap and our remapping rules. In our case there's a single rule - `Enter` acts as `Enter` on tap (when pressed briefly) and as (right) `Control` when held down longer.

Then we need to create `/etc/udevmon.yaml` (you'll need `sudo` for this):

``` yaml
# /etc/udevmon.yaml
- JOB: "intercept -g $DEVNODE | dual-function-keys -c /home/bozhidar/.dual-function-keys.yaml | uinput -d $DEVNODE"
  DEVICE:
    EVENTS:
      EV_KEY: [KEY_ENTER, KEY_RIGHTCTRL]
```

**Note:** Update the path the `.dual-function-keys.yaml` accordingly.

Finally we need to create a `systemd` service definition file for `udevmon` and start the new service:

``` yaml
# /etc/systemd/system/udevmon.service

[Unit]
Description=udevmon
Wants=systemd-udev-settle.service
After=systemd-udev-settle.service

[Service]
ExecStart=/usr/bin/nice -n -20 /usr/local/bin/udevmon -c /etc/udevmon.yaml

[Install]
WantedBy=multi-user.target
```

Now we simply have to enable the `udevmon` service our remapping will kick in:

``` shellsession
$ sudo systemctl enable --now udevmon
```

That's all! Now you can start enjoying your beloved productivity boost!

You can achieve a lot more with `dual-function-keys`, so I'd advice you to explore the
tool further. Keep hacking!

## Alternatives

Another option I considered was [xkeysnail](https://github.com/mooz/xkeysnail), which
seemed a bit simpler to setup, as it's written in Python, and even has an [example config geared towards Emacs users](https://github.com/mooz/xkeysnail/blob/master/example/config.py). You might want to check it out.

If someone's using another approach to achieve the same result I'd love to hear about it!

[^1]: Seems currently it's only packaged for Arch Linux and family (e.g. Manjaro).

---
layout: post
title: Dealing with Clipboard Issues on Windows 11 + WSLg
date: 2022-01-04 10:09 +0200
tags:
- Windows
- WSL
---

As [mentioned recently]({% post_url 2021-12-19-using-emacs-on-windows-11-with-wsl2 %}), these days I'm running a PureGKT Emacs build on Windows 11's WSL (+ WSLg).

While everything works great overall, eventually I noticed
the following issue with my setup - you can copy text from the Windows clipboard to Emacs, but
you can't do this the other way. It seems that's some [upstream bug in WSLg](https://github.com/microsoft/wslg/issues/15).

Anyways, there are a couple of simple workarounds we can use for the time being.

1. Yanking doesn't work properly, but for some reason killing works (e.g. with `C-k` or `C-w`). Not ideal but the simplest thing is to just kill and undo when you need to send some text to Windows.

2. You can shell out from Emacs to the Windows utility `clip.exe`:

``` elisp
(defun copy-selected-text (start end)
  (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
            (shell-command (concat "echo '" text "' | clip.exe")))))
```

My solution shells out to `clip.exe` from WSL and it works reliably. The only
problem with it is that you'll notice for a second the GUI of `clip.exe` every
time you use this command. I also didn't spend any time sanitizing the input -
some texts might break the shell command (e.g. something with single quotes in
it).

If someone knows more about this issue or has better ideas - comments are most welcome!

---
layout: post
title: Setting the Default Font for Emacs
date: 2023-03-16 09:50 +0200
tags:
- Basics
- Fonts
---

Even after almost 20 years of using Emacs I keep learning new things about it.
I've always known that if you're using Emacs on Linux the best way to set the default font is via the file `.Xresources`. Here's some random example:

```
Emacs.font: Iosevka Term Curly:weight=regular:size=14
```

But I've got several computers and I use Emacs on macOS, Linux (I consider WSL Linux as well) and occasionally natively on Windows. That's why I've historically preferred to use just the font in my `init.el` like this:

``` emacs-lisp
;; the font size here is huge, because it's for X on a HiDPI screen
;; X can't handle font scaling properly, therefore I just double the font size
;; when I have to use Emacs under X
(set-frame-font "Cascadia Code 28")
```

I recently, however, noticed that under X child frames are not using the same font and looked a lot smaller.[^1]

![child_frames_font.png](/assets/images/child_frames_font.png)

I was sure this had something to do with X not handling HiDPI screens properly, but it wasn't immediately obvious to me why was the font in the child frame different from the font used in the primary Emacs frame.

Turned out that `set-frame-font` by default doesn't apply to child frames of the main frame. Fortunately you can easily address this by specifying an optional `set-frame-font` parameter:

> (set-frame-font FONT &optional KEEP-SIZE FRAMES INHIBIT-CUSTOMIZE)
>
> Set the default font to FONT.
> When called interactively, prompt for the name of a font, and use
> that font on the selected frame.  When called from Lisp, FONT
> should be a font name (a string), a font object, font entity, or
> font spec.
>
> If KEEP-SIZE is nil, keep the number of frame lines and columns
> fixed.  If KEEP-SIZE is non-nil (or with a prefix argument), try
> to keep the current frame size fixed (in pixels) by adjusting the
> number of lines and columns.
>
> If FRAMES is nil, apply the font to the selected frame only.
> If FRAMES is non-nil, it should be a list of frames to act upon,
> or t meaning all existing graphical frames.
> Also, if FRAMES is non-nil, alter the user’s Customization settings
> as though the font-related attributes of the ‘default’ face had been
> "set in this session", so that the font is applied to future frames.
>
> If INHIBIT-CUSTOMIZE is non-nil, don’t update the user’s
> Customization settings.
>

So, in the end all I needed to do was:

``` emacs-lisp
(set-frame-font "Cascadia Code 28" nil t)
```

Magic! Now the child frames had exactly the same font as the rest of my Emacs!

Another Emacs lesson learned. That's all I have for you today. Keep hacking!

**P.S.** By the way, if someone knows that does `KEEP-SIZE` do exactly please share this in the comments section. I'm utterly baffled by its description.

[^1]: I finally got to trying out the popular [Corfu](https://github.com/minad/corfu) code-completion library and it's using child frames to display the completion candidates.

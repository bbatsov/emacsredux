---
layout: post
title: "Color Themes: Redux"
date: 2013-08-21 12:36
comments: true
tags:
- UI
---

## Prelude

*This is a slightly refreshed version of an
 [article originally published on my personal blog](http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/)*.

If there is something that people love as much as tweaking their
editing configurations it's probably the selection of color themes. A
good color theme can make your work much more pleasant and a bad one
that literally impair your vision. It's a fact of life that I'm a firm
supporter of low-contrast color themes with dark backgrounds - I find
them easy on the eyes and I feel that they don't strain the eyes as
much as most themes. I've even ported a couple of popular themes to
Emacs - [Zenburn](https://github.com/bbatsov/zenburn-emacs) and
[Solarized](https://github.com/bbatsov/solarized-emacs).

In this short article we'll see how color theming has changed in Emacs
24 and I'll share with you a few tips on theme creation and
distribution.

<!--more-->

## Color Theming in Emacs 24

### Theme Engines

Prior to Emacs 24 the most popular way to incorporate custom color
themes into Emacs was the
[color-theme package](http://www.emacswiki.org/emacs/ColorTheme). While
it usually got the job done it had some problems that I won't be
discussing here and more importantly - it's a third-party package,
that's not part of Emacs proper.

[Emacs 24](http://batsov.com/articles/2011/08/19/a-peek-at-emacs24/)
finally introduced a new standard way of dealing with color themes
(based on Emacs's built-in customize facility). While it doesn't have
a proper name (as far as I know) it's commonly referred to as the
`deftheme` facility, since `deftheme` is the name of the macro you'd
use to create such a theme. (`deftheme` has actually been around
since Emacs 23, but it was heavily improved in Emacs 24)

### Using Themes

Emacs 24 comes with a selection of built-in themes that you can choose
from, so you're no longer bound to the default theme (which I find
quite ugly). To choose a new theme just do a `M-x load-theme` (tab
completion is available for the names of the available themes). At
this point you can give the command a try with the `tango` theme. If you
like a theme so much that you'd want to use it all the time you can
put in your Emacs configuration (`.emacs` or `init.el` for instance) like this:

``` elisp
(load-theme 'theme-name t)
```

If you'd like to return to the default theme just do a `M-x disable-theme`.

### Creating Themes

How do you create a `deftheme` theme? Quite simply actually - just do
a **M-x customize-create-theme**. You'll be presented with an UI
prompting you for a theme name, description and faces. After you save
the theme a file called `name-theme.el` will be written on your
filesystem. Here's its skeleton:

``` elisp
(deftheme demo
  "Demo theme")

(custom-theme-set-faces
 'demo
 ;;; list of custom faces
 )

(provide-theme 'demo)
```

Personally I dislike customize a lot, so when I needed to create an
Emacs 24 theme for the first time I just opened the source code of
the built-in `tango` theme and used it as a reference.

Once you've created the new theme you'll have to drop it in a folder
that's on the `custom-theme-load-path`. I'd suggest the following:

``` elisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
```

If you're an [Emacs Prelude](https://github.com/bbatsov/prelude) user
you're already covered. This folder exists and is automatically added
to `custom-theme-load-path` by Prelude, so all you have to do is drop
there the themes you'd want to try out. Prelude also uses the
**Zenburn** theme by default.

![zenburn real](/assets/images/zenburn_real.png)

You may find the
[rainbow-mode](http://julien.danjou.info/software/rainbow-mode) useful
when developing color themes. If fontifies strings that represent
color codes according to those colors. The mode is known to be a great
addition to css-mode, but I find it very helpful with color theme
development as well. It's also included (and enabled) in Prelude by
default. Here you can see it in action.

![rainbow-mode](/assets/images/rainbow-mode.png)

I'd also advise you follow the proper naming convention
`name-theme.el` so that it's apparent that your theme is `deftheme`
compatible.

Oh, and one more thing - porting themes from `color-theme` to
`deftheme` is really simple (just have a look at the old and the new
version of Zenburn in its repo), so you should really consider porting
all the themes you maintain to `deftheme`. These days most of the
popular themes are `deftheme` only, but I guess you might still
stumble on some theme that's `color-theme` only.

### Installing Additional Themes

The Emacs package manager `package.el` (formerly known as ELPA) is
gaining a lot of popularity lately and the community repos
[MELPA](http://melpa.milkbox.net/) and
[Marmalade](http://marmalade-repo.org/) already house **many** Emacs
24 themes that you can install from there. Assuming you're using one
of the repos you can install `zenburn` like this:

```
M-x package-install RET zenburn-theme
```

After it's installed you can activate it like this:

```
M-x load-theme RET zenburn
```

### Making Themes Package.el Friendly

If you're developing a theme that you'd like to submit to MELPA &
Marmalade it's imperative that the theme modifies the
`custom-theme-load-path` in an `autoload` - otherwise it won't be of
much use. Add the following snippet (or something similar) before the
`provide-theme` line if your custom theme:

``` elisp
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
```

### A note for OSX users

For color themes to be rendered correctly you need to build your Emacs with
`srbg` support. If you're a **homebrew** user this means you have to
install Emacs like this:

```
brew install emacs --cocoa --srgb
```

The Emacs from [Emacs for MacOS X](http://emacsformacosx.com/) is
built without it and I would not recommend you using it.

Here's how Zenburn looks with `srgb` support.

![zenburn real](/assets/images/zenburn_real.png)

And this is how it looks without it.

![zenburn](/assets/images/zenburn.png)

Some colors look lighter and a bit washed out without `srgb`.

# Epilogue

Color theming in Emacs has never been easier.

Try many of the excellent themes available on MELPA & Marmalade!
Develop new themes & share them with everyone!
Port themes from `color-theme` to `deftheme` so we can finally say
goodbye to `color-theme`!

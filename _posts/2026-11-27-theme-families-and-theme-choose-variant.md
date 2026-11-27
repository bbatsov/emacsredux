---
layout: post
title: "Theme Families and theme-choose-variant"
date: 2026-11-27 14:30 +0200
tags:
- Emacs 29
- Themes
- Built-ins
---

A while ago I [wrote]({% post_url 2026-03-30-creating-emacs-color-themes %})
that multi-variant themes have become the norm - pretty much every popular theme
these days is a family of a few dark variants and a light one. And every such
family ships its own command for moving between the variants:
`modus-themes-toggle`, `ef-themes-select`, `tokyo-night-select`,
`batppuccin-select` and so on.

What I had completely missed is that Emacs has had a built-in command for this
since Emacs 29 - `theme-choose-variant`. I found out about it only recently, when
I tried it with my own
[Tokyo Night](https://github.com/bbatsov/tokyo-night-emacs) theme and got this
in return:

```
Theme `tokyo-night' does not have any known variants
```

Four variants, and Emacs knows about none of them. Let's see why.

<!--more-->

## The command

`M-x theme-choose-variant` looks at the theme that's currently enabled and
switches to another theme from the same family. If there's only one other
variant it just switches to it, otherwise it asks you which one you want. The
command is also available as `toggle-theme`, which is a much better name for the
two-variant case.

You don't need to install anything to try it, as a few of the themes that ship
with Emacs come in pairs - `tango` and `tango-dark`, `leuven` and `leuven-dark`,
`tsdh-light` and `tsdh-dark`:

```emacs-lisp
(load-theme 'leuven t)
```

`M-x toggle-theme` will take you to `leuven-dark`, and another `M-x toggle-theme`
will bring you back to `leuven`. The old theme gets disabled before the new one
is loaded, so you don't end up with one theme stacked on top of the other. (I
wrote about that problem in
[Clean Unloading of Emacs Themes]({% post_url 2025-02-03-clean-unloading-of-emacs-themes %}))

The Modus themes show the other side of the command. There are eight of them in
Emacs these days, so with `modus-operandi` enabled you won't get a toggle, but a
prompt listing the other seven. And a theme that's on its own, like `wombat`,
gives you the same error I got:

```
Theme `wombat' does not have any known variants
```

## How themes opt in

How does Emacs know that `leuven` and `leuven-dark` belong together? It doesn't
guess from the names. Emacs 29 taught `deftheme` to accept a property list
after the docstring, and the theme has to say what family it's a part of:

```emacs-lisp
(deftheme leuven-dark
  "Face colors with a dark background."
  :background-mode 'dark
  :family 'leuven
  :kind 'color-scheme)
```

- `:family` is what ties the variants together. Themes with the same family are
  variants of each other.
- `:kind` should be `color-scheme` for a regular color theme. Themes that don't
  set it are treated as color schemes for the time being.
- `:background-mode` is `light` or `dark`. Nothing uses it yet, but the manual
  asks theme authors to set it anyway.

`wombat` sets a kind and a background mode, but no family. My themes didn't set
anything at all - their `deftheme` forms are just a name and a docstring.

## The autoload catch

Adding the properties to `deftheme` is not enough, though. They are stored on the
theme's symbol when the theme file gets evaluated, and nobody has evaluated the
files of the variants you haven't loaded yet. Emacs has to know the properties
of every variant before any of them is loaded.

The built-in themes solve this with a special cookie:

```emacs-lisp
;;;###theme-autoload
(deftheme leuven-dark
  ...
```

When Emacs is built, the properties of every theme marked this way are
extracted into a file named `theme-loaddefs.el`, that's loaded at startup:

```emacs-lisp
(put 'leuven-dark 'theme-properties
     (list :background-mode 'dark :family 'leuven :kind 'color-scheme))
```

That cookie means nothing in a package you've installed from MELPA, though. A
third-party theme has to use the regular `;;;###autoload` cookie in front of its
`deftheme`. Starting with Emacs 29 the autoload generator knows about `deftheme`
and extracts only the properties from it, so you end up with the same `put` form
in the package's autoloads file.

Without this the command will find only the variants that you've already loaded
in the current Emacs session, which for most people means none of them:

```
`foo-dark' has no variants
```

## Who supports this?

I went over the source of 60 or so popular themes to see how many of them have
picked this up, and the short answer is "very few". Protesilaos's themes (Modus,
Ef, Standard, Doric) declare a family, and so do `doom-themes` and a handful of
smaller packages. Pretty much everyone else declares nothing, including big
multi-variant families like Solarized, Gruvbox and Kanagawa.

What surprised me more is that I didn't find a single third-party theme that
autoloads its properties. As far as I can tell that leaves the built-in command
blind to the variants you haven't loaded yet, which is probably why the themes
that did the work kept their own commands anyway. Modus, for instance, quietly
loads its themes (without enabling them) to learn their properties.

## A few rough edges

The command is simple, and it shows:

- It toggles only when the family has exactly two themes. Tokyo Night has four,
  so I'd get a prompt every time. There's no way to say "these two are the ones
  I switch between".
- It doesn't know what's light and what's dark, because nothing reads
  `:background-mode`. You can't ask it for "the light one".
- It refuses to work if you have more than one theme enabled.
- It loads the new variant with a plain `load-theme`, so for a third-party theme
  you haven't marked as safe you'll get the usual "Loading a theme can run Lisp
  code" question.

That last one is the reason `modus-themes-toggle` is still around. Protesilaos
considered dropping it in favor of `toggle-theme`, and decided against it
because of the prompts.

Also keep in mind that this is a command you invoke by hand. If you want Emacs
to follow the appearance of your OS or the time of the day, see
[Automatic Light/Dark Theme Switching]({% post_url 2026-03-29-automatic-light-dark-theme-switching %}).

## What about older Emacs versions?

The good news is that the properties are harmless there. `deftheme` has been
ignoring any extra arguments for ages (I checked all the way back to Emacs 24),
so a theme that declares a family will load just fine on Emacs 27 or 28.

That's about all the good news. `theme-choose-variant` doesn't exist before
Emacs 29, so the users of the older versions get nothing out of the
properties. The autoload trick doesn't translate either - the old autoload
generator doesn't know about `deftheme`, so it copies the whole form into the
autoloads file and the theme gets declared at startup.

Both Tokyo Night and
[Batppuccin](https://github.com/bbatsov/batppuccin-emacs) support Emacs 27.1,
which means I can't rely on the built-in command and I need one of my own
anyway. It's not much code:

```emacs-lisp
(defconst tokyo-night--variants
  '(tokyo-night tokyo-night-storm tokyo-night-moon tokyo-night-day))

;;;###autoload
(defun tokyo-night-select ()
  "Select and load a Tokyo theme variant interactively."
  (interactive)
  (let* ((names (mapcar #'symbol-name tokyo-night--variants))
         (choice (intern (completing-read "Tokyo theme: " names nil t))))
    (mapc #'disable-theme tokyo-night--variants)
    (load-theme choice t)
    (setq tokyo-night--current choice)
    (run-hook-with-args 'tokyo-night-after-load-hook choice)))
```

The theme knows its own variants, so there are no properties to autoload and no
theme directories to scan. It works the same on every Emacs version, it doesn't
ask you to confirm anything, and it can do things that only make sense for this
theme, like running a hook after the switch. That's why I think theme authors
who still support older Emacs versions shouldn't build on
`theme-choose-variant` for now. Ship your own command, as most themes already
do.

The two approaches don't exclude each other, of course. Declaring a family costs
a few lines per variant, and it makes the theme play nice with the built-in
command on Emacs 29+. I'll be adding the properties to my themes, and their own
commands are staying.

Do you use `theme-choose-variant`? Did you even know it existed? I'd love to
hear about it in the comments!

That's all I have for you today. Keep hacking!

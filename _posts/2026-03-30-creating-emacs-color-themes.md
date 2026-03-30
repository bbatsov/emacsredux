---
layout: post
title: "Creating Emacs Color Themes, Revisited"
date: 2026-03-30 08:25 +0300
tags:
- Themes
- Packages
---

Creating Emacs color themes is a topic I hadn't thought much about in recent
years. My first theme ([Zenburn](https://github.com/bbatsov/zenburn-emacs))
has been in maintenance mode for ages, and
[Solarized](https://github.com/bbatsov/solarized-emacs) mostly runs
itself at this point. But working on my ports of
[Tokyo (Night) Themes](https://github.com/bbatsov/emacs-tokyo-themes) and
[Catppuccin (Batppuccin)](https://github.com/bbatsov/batppuccin-emacs) made me
re-examine the whole topic with fresh eyes. The biggest shift I've noticed is
that multi-variant themes (light/dark/high-contrast from a shared codebase) have
become the norm rather than the exception, and that pattern naturally leads to
reusable theming infrastructure.

The task has always been simultaneously easy and hard. Easy because `deftheme`
and `custom-theme-set-faces` are well-documented and do exactly what you'd
expect. Hard because the real challenge was never the mechanics -- it's knowing
*which* faces to theme and keeping your color choices consistent across hundreds
of them.

**Note:** In Emacs, a [face](https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html)
is a named set of visual attributes -- foreground color, background, bold,
italic, underline, etc. -- that controls how a piece of text looks. Themes work
by setting faces to match a color palette. See also the Elisp manual's section
on [custom themes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Custom-Themes.html)
for the full API.

<!--more-->

## The Classic Approach

The traditional way to create an Emacs theme is to write a `deftheme` form,
then set faces one by one with `custom-theme-set-faces`:

``` emacs-lisp
(deftheme my-cool-theme "A cool theme.")

(custom-theme-set-faces
 'my-cool-theme
 ;; The `t` means "all display types" -- you can also specify different
 ;; colors for different displays (GUI vs 256-color terminal, etc.)
 '(default ((t (:foreground "#c0caf5" :background "#1a1b26"))))
 '(font-lock-keyword-face ((t (:foreground "#bb9af7"))))
 '(font-lock-string-face ((t (:foreground "#9ece6a"))))
 ;; ... 200+ more faces
 )

(provide-theme 'my-cool-theme)
```

This works fine and gives you total control. Many excellent themes are built
exactly this way. In practice, a lot of new themes start their life as copies of
existing themes -- mostly to avoid the leg-work of discovering which faces to
define. You grab a well-maintained theme, swap the colors, and you're halfway
there.

That said, the approach has a couple of pain points:

- You need to know what faces exist. Emacs has dozens of built-in faces, and
  every popular package adds its own. Miss a few and your theme looks polished
  in some buffers but broken in others. `list-faces-display` is your friend
  here, but it only shows faces that are currently loaded.
- Consistency is on you. With hundreds of face definitions, it's easy to use
  slightly different shades for things that should look the same, or to pick a
  color that clashes with your palette. Nothing enforces coherence -- you have
  to do that yourself.
- Maintaining multiple variants is tedious. Want a light and dark version?
  You're duplicating most of the face definitions with different colors.[^1]

One more gotcha: some packages use variables instead of faces for their colors
(e.g., `hl-todo-keyword-faces`, `ansi-color-names-vector`). You can set those
with `custom-theme-set-variables`, but you have to know they exist first. It's
easy to think you've themed everything via faces and then discover a package
that hard-codes colors in a defcustom.

How big of a problem the face tracking is depends on your scope. If you only
care about built-in Emacs faces, it's pretty manageable -- that's what most of
the bundled themes do (check `wombat`, `deeper-blue`, or `tango` -- they
define faces almost exclusively for packages that ship with Emacs and don't
touch third-party packages at all). But if you want your theme to look good in
`magit`, `corfu`, `vertico`, `transient`, and a dozen other popular packages,
you're signing up for ongoing maintenance. A new version of `magit` adds a face
and suddenly your theme has gaps you didn't know about.

I still do things this way for Tokyo Themes and Batppuccin, but the more themes
I maintain the more I wonder if that's overkill.

## Every Multi-Variant Theme Is a Mini Framework

Here's something worth pointing out: any theme that ships multiple variants is
already a framework of sorts, whether it calls itself one or not. The moment you
factor out the palette from the face definitions so that multiple variants can
share the same code, you've built the core of a theming engine.

Take Tokyo Themes as an example. There are four variants (night, storm, moon, day), but the face
definitions live in a single shared file (`tokyo-themes.el`). Each variant is a
thin wrapper -- just a `deftheme`, a palette alist, and a call to the shared
`tokyo--apply-theme` function:

``` emacs-lisp
(require 'tokyo-themes)
(deftheme tokyo-night "A clean dark theme inspired by Tokyo city lights.")
(tokyo--apply-theme 'tokyo-night tokyo-night-colors-alist)
(provide-theme 'tokyo-night)
```

That's the entire theme file. The palette is defined elsewhere, and the face
logic is shared -- which is exactly how you solve the variant duplication problem
mentioned earlier. In theory, anyone could define a new palette alist and call
`tokyo--apply-theme` to create a fifth variant. The infrastructure is already
there -- it's just not explicitly marketed as a "framework."

This is exactly how the theming features of packages like Solarized and
Modus evolved. They started as regular themes, grew variants, factored out
the shared code, and eventually exposed that machinery to users.

## Meta-Themes

Some theme packages went a step further and turned their internal infrastructure
into an explicit theming API.

### Solarized

[solarized-emacs](https://github.com/bbatsov/solarized-emacs) started as a
straight port of Ethan Schoonover's Solarized palette, but over time it grew
the ability to create entirely new themes from custom palettes. You can use
`solarized-create-theme-file-with-palette` to generate a new theme by supplying
just 10 colors (2 base + 8 accent) -- it derives all the intermediate shades
and maps them to the full set of faces:

``` emacs-lisp
(solarized-create-theme-file-with-palette 'dark 'my-solarized-dark
  '("#002b36" "#fdf6e3"                         ;; base colors
    "#b58900" "#cb4b16" "#dc322f" "#d33682"     ;; accents
    "#6c71c4" "#268bd2" "#2aa198" "#859900"))
```

This is how Solarized's own variants (dark, light, gruvbox, zenburn, etc.) are
built internally. I'll admit, though, that I always found it a bit weird to ship
themes like Gruvbox and Zenburn under the Solarized umbrella. If you install
`solarized-emacs` and find a `solarized-gruvbox-dark` theme in the list, the
natural reaction is "wait, what does Gruvbox have to do with Solarized?" The
answer is "nothing, really -- they just share the theming engine." That makes
perfect sense once you understand the architecture, but I think it's confusing
for newcomers. It was part of the reason I was never super excited about this
direction for `solarized-emacs`.

### Modus Themes

The [modus-themes](https://github.com/protesilaos/modus-themes) take a
different approach. Rather than generating new theme files, they offer deep
runtime customization through palette overrides:

``` emacs-lisp
(setq modus-themes-common-palette-overrides
      '((bg-main "#1a1b26")
        (fg-main "#c0caf5")
        (keyword magenta-warmer)))
```

You can override any named color in the palette without touching the theme
source. The result feels like a different theme, but it's still Modus under the
hood with all its accessibility guarantees. The overrides apply to whichever
Modus variant you load, and `modus-themes-toggle` switches between variants
while keeping your overrides intact. Protesilaos's
[ef-themes](https://github.com/protesilaos/ef-themes) share the same
architecture.

## Theming Frameworks

If you want to create something brand new rather than customize an existing
theme family, there are a couple of frameworks designed for this.

### Autothemer

[autothemer](https://github.com/jasonm23/autothemer) provides a macro that
replaces the verbose `custom-theme-set-faces` boilerplate with a cleaner,
palette-driven approach:

``` emacs-lisp
(autothemer-deftheme
 my-theme "A theme using autothemer."
 ;; Display classes: 24-bit GUI, 256-color terminal, 16-color terminal
 ((((class color) (min-colors 16777216)) ((class color) (min-colors 256)) t)
  (my-bg    "#1a1b26" "black"   "black")
  (my-fg    "#c0caf5" "white"   "white")
  (my-red   "#f7768e" "red"     "red")
  (my-green "#9ece6a" "green"   "green"))

 ;; Face specs -- just reference palette names, no display class noise
 ((default         (:foreground my-fg :background my-bg))
  (font-lock-keyword-face (:foreground my-red))
  (font-lock-string-face  (:foreground my-green))))
```

You define your palette once as named colors with fallback values for different
display capabilities (GUI frames and terminals support different color depths,
so themes need appropriate fallbacks for each). Then you reference those names in
face specs without worrying about display classes again. Autothemer also provides
some nice extras like SVG palette previews and helpers for discovering unthemed
faces.

### Base16 / Tinted Theming

[base16-emacs](https://github.com/tinted-theming/base16-emacs) is part of the
larger [Tinted Theming](https://github.com/tinted-theming) ecosystem. The idea
is that you define a scheme as 16 colors in a YAML file, and a builder
generates themes for Emacs (and every other editor/terminal) from a shared
template. You don't write Elisp at all -- you write YAML and run a build step.

This is great if you want one palette to rule all your tools, but you give up
fine-grained control over individual Emacs faces. The generated themes cover a
good set of faces, but they might not handle every niche package you use.

## From Scratch vs. Framework: Pros and Cons

|                       | From Scratch                       | Meta-Theme / Framework                    |
|-----------------------|------------------------------------|-------------------------------------------|
| Control               | Total -- every face is yours       | Constrained by what the framework exposes |
| Consistency           | You enforce it manually            | The framework helps (palette-driven)      |
| Coverage              | You add faces as you discover them | Inherited from the base theme/template    |
| Maintenance           | You track upstream face changes    | Shifted to the meta-theme maintainers     |
| Multiple variants     | Duplicate or factor out yourself   | Built-in support                          |
| Learning curve        | Just `deftheme`                    | Framework-specific API                    |

## When to Use What

I guess relatively few people end up creating theme packages, but here's a
bit of general advice for them.

If you want total control over every face and you're willing to put in the
maintenance work, roll your own. This makes sense for themes with a strong
design vision where you want to make deliberate choices about every element.
It's more work, but nothing stands between you and the result you want.

If you mostly like an existing theme but want different colors, customizing a
meta-theme (Modus, Solarized, ef-themes) is a good bet. You get battle-tested
face coverage for free, and the palette override approach means you can tweak
things without forking. Keep in mind, though, that the face coverage problem
doesn't disappear -- you're just shifting it to the meta-theme maintainers. How
comprehensive and up-to-date things stay depends entirely on how diligent they
are.

If you're creating something new but don't want to deal with the boilerplate,
use a framework. Autothemer is the best fit if you want to stay in Elisp and
have fine control. Base16/Tinted Theming is the pick if you want one palette
definition across all your tools.

## Parting Thoughts

I'm still a "classic" -- I like rolling out my themes from scratch. There's
something satisfying about hand-picking the color for every face. But I won't
pretend it doesn't get tedious, especially when you maintain several themes
across multiple variants. Every time a package adds new faces, that's more
work for me.

If I were starting fresh today, I'd seriously consider Autothemer or building on
top of a meta-theme (extracted from my existing theme packages). The time you
save on maintenance is time you can spend on what actually matters -- making
your theme look good.

On the topic of maintenance -- one area where AI tools can actually help is
extracting the relevant faces from a list of packages you want to support.
Instead of loading each package, running `list-faces-display`, and eyeballing
what's new, you can ask an LLM to scan the source and give you the face
definitions. It's also handy for periodically syncing your theme against the
latest versions of those packages to catch newly added faces. Not glamorous
work, but exactly the kind of tedium that AI is good at.

That's all I have for you today. Keep hacking!

[^1]: Later on you'll see that's a pretty easy problem to address.

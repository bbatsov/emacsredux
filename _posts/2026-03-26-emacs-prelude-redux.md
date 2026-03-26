---
layout: post
title: "Emacs Prelude: Redux"
date: 2026-03-26 13:50 +0200
tags:
- Prelude
---

> Programmers know the benefits of everything and the tradeoffs of nothing.
>
> -- Rich Hickey

Earlier today I wrote about [Emacs Redux turning 13]({% post_url 2026-03-26-happy-13th-birthday-emacs-redux %}). That felt like the perfect
occasion to also ship something I've been working towards for a while --
[Emacs Prelude](https://github.com/bbatsov/prelude) 2.0.

<!--more-->

## A Long Time Coming

The last tagged Prelude release (1.1) happened all the way back in February 2021.
Five years without a release might sound alarming, but I'd argue it's a
feature, not a bug. Prelude has always aimed to be a *foundation* -- simple,
stable, easy to understand. I never wanted users to dread pulling upstream
because everything moved under their feet. If you look at some of the more
"sophisticated" Emacs distributions out there, the constant churn and complexity
can be genuinely overwhelming. That's not the experience I want for Prelude
users.

That said, five years is a long time in Emacs land. Emacs 29 landed with
built-in tree-sitter, Eglot, and `use-package`. A bunch of third-party packages
that Prelude depended on became obsolete or unmaintained. It was time for a
proper update.

## What's New

Prelude 2.0 is all about modernizing the distribution for the Emacs 29+ era.

### Emacs 29.1 is now the minimum version

This was the big enabling change. Emacs 29 brought so many things that Prelude
previously had to install or polyfill -- `use-package`, `display-line-numbers-mode`,
`isearch-lazy-count`, `use-short-answers`, tree-sitter, Eglot -- that bumping
the minimum version let me drop a ton of compatibility code and third-party
dependencies in one go. Packages like `nlinum`, `anzu`, and `epl` are gone
entirely, replaced by their built-in equivalents.

### Tree-sitter support

Language modules now automatically use tree-sitter modes (e.g., `python-ts-mode`
instead of `python-mode`) when a grammar is available, with graceful fallback to
classic modes when it isn't. This means better syntax highlighting and
structural editing with zero configuration -- just install the grammar and
you're done. Prelude currently supports tree-sitter remapping for C/C++, Go,
Python, JavaScript, TypeScript (including TSX), Ruby, Elixir, Shell, YAML, and
CSS. Some modules like `prelude-ocaml` (which uses `neocaml`) are tree-sitter-only
by design.

### Built-in LSP via Eglot

Most language modules now come with LSP support out of the box, using Eglot as
the default client. No extra packages to install, no configuration to write --
just make sure you have the right language server on your `$PATH` and Prelude
handles the rest. Eglot keybindings live under the `C-c C-l` prefix (rename,
code actions, format, organize imports), consistent with what lsp-mode users
are used to. If you prefer lsp-mode, set `prelude-lsp-client` to `'lsp-mode`
in your personal config and Prelude will use it across all language modules
instead.

### Modernized language modules

Python, JavaScript, TypeScript, OCaml, Go, and others have been updated to use
modern tooling. `anaconda-mode` is replaced by LSP, `js2-mode` by `js-ts-mode`,
`tide` by `typescript-ts-mode`, `tuareg` by `neocaml`, `alchemist` and
`go-projectile` are gone (both unmaintained for years). The goal was to bring
every language module up to 2026 standards while keeping them short and
focused -- most are still under 50 lines.

### Faster startup

I still stand by my [older take that Emacs startup time doesn't really
matter](https://batsov.com/articles/2025/04/07/emacs-startup-time-does-not-matter/) --
you start Emacs once and it runs for days (or weeks, or months). But when the
fruit hangs this low, why not pick it? Interactive packages are now loaded lazily
via `use-package` `:defer`, and redundant `require` calls have been eliminated
throughout. The old `defadvice` calls have been replaced with modern
`define-advice` / `advice-add`, and a fair amount of dead code has been cleaned
up across the board. Nothing dramatic, but it all adds up to a noticeably
snappier startup for those who care about such things.

There's a detailed [changelog](https://github.com/bbatsov/prelude/blob/master/CHANGELOG.md)
if you want the full picture, and a
[migration guide](https://github.com/bbatsov/prelude#upgrading-to-prelude-20) in the
README to help with the upgrade.

## The Docs Got a Facelift

The [documentation site](https://prelude.emacsredux.com) has been updated and
now uses the Material for MkDocs theme, which is a lot nicer to read and
navigate than the old ReadTheDocs default. The content has been refreshed too,
with all modules now properly documented.

## What's Next

There's more I'd like to do. For instance, I haven't yet pushed to convert
everything to use `use-package` idiomatically -- some modules still use the
old `with-eval-after-load` / `add-hook` style. I'd also like to explore deeper
integration with `project.el` and perhaps revisit the module system itself. But
everything is in good shape overall, and I'd rather ship incremental
improvements than hold back a release for perfection.

## Starter Kits in the Age of AI

A fair question to ask in 2026 is whether Emacs distributions even matter
anymore. With tools like Claude Code, you can just ask an AI to set up Emacs
however you like -- generate an `init.el` from scratch, configure LSP, pick a
completion framework, wire up keybindings. Why bother with a starter kit?

I think there are a few reasons Prelude (and projects like it) still matter.

First, AI coding agents are only as good as the code they've been trained on.
And right now, the Emacs ecosystem has a serious "popularity inertia" problem --
agents will almost always suggest the older, more established package over a
newer alternative, even when the newer one is clearly better. Ask an AI to set
up OCaml in Emacs and you'll get `tuareg` + `merlin` every time, not `neocaml` +
`ocaml-eglot`. Ask for a completion framework and you'll get `ivy` or `helm`,
not `vertico` + `marginalia`. The training data reflects the past, not the
present. Well-maintained distributions that track the state of the art serve as
a corrective -- both for humans browsing GitHub and for the models trained on
it.

Second, there's real value in curation. An AI can generate a config, but it
can't tell you which packages play well together, which ones are unmaintained,
or which defaults will bite you six months from now. That kind of judgment comes
from experience, and it's exactly what a good starter kit encodes.

And third, simplicity still wins. A generated config you don't understand is
worse than a short, readable one you do. Prelude's modules are deliberately
small and straightforward -- they're meant to be read, forked, and modified. I'd
rather give someone 20 lines of well-chosen defaults than a 200-line AI-generated
config full of cargo-culted settings.

I wrote more about this topic in [Emacs and Vim in the Age of
AI](https://batsov.com/articles/2026/03/09/emacs-and-vim-in-the-age-of-ai/) if
you're curious.

## Prelude and Emacs Redux

Emacs Prelude holds a special place in my heart. It was one of my first
open-source projects -- I started it back in 2011, two years before this blog
even existed. When I launched Emacs Redux in 2013, many of my early posts were
essentially showcasing features and ideas from Prelude. The two projects grew
up together, and in many ways Prelude was the proving ground for the tips and
workflows that ended up here. It's fitting that they celebrate together today.

## The Return of the Prelude

> Simplicity is a great virtue but it requires hard work to achieve it and
> education to appreciate it. And to make matters worse: complexity sells better.
>
> -- Edsger W. Dijkstra

I've always believed that slow, deliberate change beats constant reinvention.
It's not glamorous, it doesn't generate hype, but it builds something you can
actually rely on. Prelude doesn't try to be everything to everyone -- it tries
to be a solid, understandable starting point that respects your time and
attention.

And here's a fun bit of trivia to close on: 2026 happens to be the year Honda
brings back the [Prelude](https://en.wikipedia.org/wiki/Honda_Prelude). Very few
people know this, but I was actually considering buying a (pretty old) Honda
Prelude around the time I created Emacs Prelude back in 2011 -- that's where the
name came from! I never did buy the car, but the Emacs distribution turned out
to be a much better investment.[^1] And now, 15 years later, both Preludes are making
a comeback. Sometimes things just come full circle.

That's all I have for you today. Keep hacking!

[^1]: More trivia for you - I did end up buying a BMW E39 in 2010 instead of the Prelude. I still own it and it just turned 26 earlier this month!

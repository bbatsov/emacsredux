---
layout: post
title: Replacing Flyspell with Jinx
date: 2026-07-13 08:26 +0300
tags:
- Packages
- Flyspell
- Jinx
---

I've been using `flyspell-mode` for the better part of two decades, and I've
written about it [a couple of times]({% post_url 2025-03-31-essential-flyspell %})
[before]({% post_url 2019-05-24-spell-checking-comments %}). It gets the job
done, but it has always felt a bit creaky to me - it checks words one at a time
as you type, `flyspell-buffer` is painfully slow in big buffers, and you have to
remember to enable `flyspell-prog-mode` in your programming modes, so it would
check only comments and strings there.

Recently, as part of the ongoing overhaul of my [personal Emacs
config](https://github.com/bbatsov/emacs.d), I finally replaced it with
[jinx](https://github.com/minad/jinx) and I can already tell you that I'm not
going back.

<!--more-->

## Why Jinx?

Jinx is a modern spell-checker by Daniel Mendler (of
[vertico](https://github.com/minad/vertico),
[consult](https://github.com/minad/consult) and
[corfu](https://github.com/minad/corfu) fame), built on top of
[libenchant](https://rrthomas.github.io/enchant/).[^1] Quite a few things make
it a great alternative to Flyspell:

- It's fast. Jinx checks only the visible part of the buffer (it hooks into
  Emacs's JIT font-locking machinery), so it doesn't matter if your buffer is
  100 lines or 100,000 lines long. There's no need for anything like
  `flyspell-buffer` - misspellings simply get highlighted as they scroll into
  view.
- One mode everywhere. `global-jinx-mode` replaces both `flyspell-mode` and
  `flyspell-prog-mode`. Jinx decides what to check based on faces, so in
  programming modes it automatically limits itself to comments and strings.
- Enchant is a facade over many spell-checking backends - Hunspell, Nuspell,
  Aspell, and (notably) AppleSpell on macOS. That last one means Jinx can use
  macOS's built-in dictionaries and you don't have to install any yourself.
- Support for multiple languages at once. Set `jinx-languages` to something
  like `"en_US bg"` and Jinx will check both English and Bulgarian in the same
  buffer. As someone who writes in two languages every day, this alone would
  have sold me on it.
- A much nicer correction UI. `M-$` (`jinx-correct`) pops up a
  `completing-read` menu (lovely if you're using `vertico`) with the
  suggestions, and below them - entries for saving the word to your personal
  dictionary, as a file-local word, or just for the current session. `C-u M-$`
  corrects all the misspellings in the buffer in one go.

## The Setup

Here's the relevant bit of my config:

``` emacs-lisp
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))
```

`M-$` is bound to `ispell-word` by default, so rebinding it to `jinx-correct`
feels quite natural. `jinx-languages` allows you to switch the active languages
for the current buffer on the fly.

One thing to keep in mind - Jinx uses a small native module to talk to
libenchant, and this module gets compiled automatically the first time you
enable the mode. In other words - you'll need `libenchant` (plus `pkgconf`) and
a C compiler on your system:

``` shell
# macOS
brew install enchant

# Debian/Ubuntu
sudo apt install libenchant-2-dev pkgconf
```

Admittedly, that's a bit more setup than Flyspell, which is built-in and only
needs an external `aspell`/`hunspell` binary. Flyspell will probably remain the
path of least resistance, but in my opinion the small extra effort pays for
itself many times over.

**Note:** Dropping Flyspell also frees up `C-.` and `C-;`, which happen to be
prime keybinding real estate. In my setup they are now bound to
[embark](https://github.com/oantolin/embark)'s `embark-act` and `embark-dwim`,
but that's a topic for another article.

## Closing Thoughts

Funny enough, I knew about Jinx for quite a while, but I kept using Flyspell
mostly because of inertia. I guess old habits die hard! Now that I've finally
made the switch, I can't help but wonder what took me so long.

Have you tried Jinx already? Are you still happily using Flyspell (or something
else entirely)? I'd love to hear your thoughts in the comments!

That's all I have for you today. Keep fixing those typos!

[^1]: I guess the name makes sense - what do you get from an enchantment gone wrong? A jinx!

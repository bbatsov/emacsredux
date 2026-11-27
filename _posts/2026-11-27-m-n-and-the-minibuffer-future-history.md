---
layout: post
title: "M-n: The Minibuffer's Future History"
date: 2026-11-27 08:30 +0200
tags:
- Minibuffer
- Built-ins
---

Every Emacs user knows that pressing `M-p` in the minibuffer goes back through
the history of the things they've typed there before. That's one of the first
things you learn, right after `C-g`. And if you go back too far you can press
`M-n` to go forward again. End of story, right?

Not quite! What happens if you press `M-n` right away, when there's nothing to
go forward to? You might expect an error, but instead Emacs will often insert
something useful. That's what the Emacs manual calls "future history" - a list
of things that the command guessed you might want to type next. [^1]

## A Few Examples

Here are a few built-in commands where `M-n` does something handy:

- `C-x C-f` (`find-file`) - inserts the file name at point (if there's one) and
  after it the name of the file you're visiting, which is great when you want
  to open one of its siblings
- `M-%` (`query-replace`) - inserts the symbol at point and after it the last
  thing you searched for with `isearch`
- `M-s o` (`occur`) - the same suggestions, plus the active region
- `M-!` (`shell-command`) - inserts the name of the file you're visiting (or
  the file at point in Dired), which is super handy for commands that operate
  on the current file
- `C-x b` (`switch-to-buffer`) - inserts the default buffer and afterwards
  keeps going through the rest of the buffers

The last one shows something that applies to every command that uses
`completing-read`. When you run out of default values, `M-n` starts going through
the completion candidates themselves. So in a pinch `M-n` is a poor man's
candidate cycling, even if you're not using any fancy completion framework.

**Tip:** A lot of prompts show their default value (e.g. `Describe function
(default car):`) and most people just press `RET` to accept it. If the default is
almost what you want, though, press `M-n` to insert it into the minibuffer and
edit it there.

## Using it in Your Own Commands

If you're writing Emacs Lisp you get all of this pretty much for free. The
functions that read from the minibuffer (`read-string`, `read-from-minibuffer`,
`completing-read`, `read-shell-command` and friends) accept a default value,
and that default value can also be a list. The first element is the default and
`M-n` will go over the rest of them in order:

``` emacs-lisp
(defun my-search (term)
  (interactive
   (list (read-string
          "Search for: " nil nil
          (delq nil
                (list (thing-at-point 'symbol t)
                      (add-log-current-defun)
                      (and buffer-file-name
                           (file-name-base buffer-file-name)))))))
  (message "Searching for %s" term))
```

Here `M-n` will first offer the symbol at point, then the name of the current
function and finally the name of the current file. There's also `read-regexp`,
which adds a bunch of standard suggestions (the ones you saw in `occur`) to
whatever you give it.

That's the trick behind most of the
[smarter prompts]({% post_url 2026-11-26-smarter-minibuffer-prompts-in-projectile %})
in the upcoming Projectile 3.5. I like this approach a lot, because the
suggestions don't get in anyone's way - if you never press `M-n` the prompt
behaves exactly as it always has.

## Lazy Suggestions

There's one catch with the list approach - you have to compute all the
suggestions before the prompt shows up, even though most of the time nobody
will ask for them. If they are expensive to compute that's a problem. Enter
`minibuffer-default-add-function`. Emacs calls it the first time `M-n` runs out
of default values, and whatever it returns becomes the new list of defaults:

``` emacs-lisp
(minibuffer-with-setup-hook
    (lambda ()
      (setq-local minibuffer-default-add-function
                  (lambda ()
                    (append (my-expensive-guesses)
                            (minibuffer-default-add-completions)))))
  (completing-read "Find file: " files))
```

`minibuffer-default-add-completions` is what this variable is set to out of the
box, and it's the reason why `M-n` goes through the completion candidates in
the first place. Here we just put our own guesses in front of them. Keep in mind
that the function gets called with the minibuffer as the current buffer, so if
your guesses depend on the buffer the command was invoked from (e.g. you want
the thing at point) you'll have to switch to it first.
`(with-selected-window (minibuffer-selected-window) ...)` does the job. That's
how `find-file` comes up with the file name at point, by the way, and I'm using
the same trick for Projectile's `projectile-find-file`.

That's all I have for you today. Give `M-n` a try the next time Emacs asks you
something - you might be pleasantly surprised! Keep hacking!

[^1]: See `C-h f next-history-element` and the [Minibuffer History](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-History.html) section of the manual.

---
layout: post
title: Goodbye setq, hello setopt!
date: 2025-04-06 19:14 +0300
tags:
- Customize
- Emacs 29
---

For many years most Emacs users used `setq` to set the various configuration options of
Emacs and the packages that they were using. This probably wasn't the best option (read on),
but it was the most popular way of doing things. Now, however, it's finally time for a change![^1]

## Why do we need setopt?

In Emacs 29, a new macro `setopt` was introduced to provide a more appropriate method for setting user options (variables defined with `defcustom`).

As mentioned above, traditionally, Emacs users have employed `setq` to assign values to variables. However, `setq` does not invoke any custom setter functions associated with user options, which can lead to unexpected behavior. Here's example of such a setter function from `copilot.el`:

```emacs-lisp
(defun copilot--lsp-settings-changed (symbol value)
  "Restart the Copilot LSP due to SYMBOL changed to VALUE.

This function will be called by the customization framework when the
`copilot-lsp-settings' is changed.  When changed with `setq', then this function
will not be called."
  (let ((was-bound (boundp symbol)))
    (set-default symbol value)
    (when was-bound
      ;; Notifying the agent with the new value does only work if we include the
      ;; last value (as nil) as well. For example, having the value
      ;; '(:github-enterprise (:uri "https://example2.ghe.com")) and setting it
      ;; to nil would require to send the value '(:github-enterprise (:uri nil))
      ;; to the server. Otherwise, the value is ignored, since sending nil is
      ;; not enough.
      (copilot--start-agent))))

(defcustom copilot-lsp-settings nil
  "Settings for the Copilot LSP server.

This value will always be sent to the server when the server starts or the value
changes.  See
https://github.com/github/copilot-language-server-release?tab=readme-ov-file#configuration-management
for complete documentation.

To change the value of this variable, the customization framework provided by
Emacs must be used.  Either use `setopt' or `customize' to change the value.  If
the value was set without the customization mechanism, then the LSP has to be
manually restarted with `copilot-diagnose'.  Otherwise, the change will not be
applied.

For example to use GitHub Enterprise use the following configuration:
 '(:github-enterprise (:uri \"https://example.ghe.com\"))

Exchange the URI with the correct URI of your organization."
  :set #'copilot--lsp-settings-changed
  :type 'sexp
  :group 'copilot
  :package-version '(copilot . "0.2"))
```

In case it's not obvious - the important thing is the `:set` property of `copilot-lsp-settings`.
Basically, every this this option is changed, a callback function should be invoked, but this won't
happen if you make the change using `setq`.

The `setopt` macro addresses this by ensuring that when you set a user option,
any associated setter functions are properly called, maintaining the integrity
of the option's behavior.

Even more importantly for me - `setopt` also checks whether the value is valid
for the user option. For instance, using `setopt` to set a user option defined
with a number type to a string will signal an error.
I'm pretty sure this will prevent a lot of (weird) configuration issues going forward! (and
inspire more package authors to declare their `defcustom`s properly)

Now let's update a bit of legacy code to use `setopt`:

```emacs-lisp
(setq user-full-name "Bozhidar Batsov"
      user-mail-address "bozhidar@emacsninja.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)
```

This will be become:

```emacs-lisp
(setopt user-full-name "Bozhidar Batsov"
        user-mail-address "bozhidar@emacsninja.com")

;; Always load newest byte code
(setopt load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setopt gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setopt large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setopt confirm-kill-processes nil)
```

Pretty shocking, right?

## When to Use What?

The introduction of `setopt` has sparked discussions within the Emacs community
regarding the best practices for setting variables. Some users have expressed
uncertainty about when to use `setq`, `customize-set-variable`, or the new
`setopt`. My take on the subject is pretty simple:

- Use `setopt` for user options to ensure that any custom setter functions are invoked.
  - It has shorter name then `customize-set-variable` and can be used to set multiple options just like `setq`.
  - Shows a warning when a configuration value does not match its `:type` specification.
  - Unlike `setq`, it does not complain when a variable is not declared. (which is quite normal when dealing with a lot of autoloaded packages)
- Use `setq` only for variables that are not defined in terms of `defcustom`.
  - Amusingly, `setopt` will work with regular variables as well, but it won't be as efficient as `setq`. Not to mention using it in such a way will be super confusing!

The way I see it, unless you're running an older Emacs version, and you're not using
`setopt` extensively in your Emacs config, you're missing out!

## Further Reading

For more detailed discussions and perspectives on this topic, check out:

- [Reddit Thread: Why was `setopt` introduced in Emacs 29?](https://www.reddit.com/r/emacs/comments/178em7u/why_was_setopt_introduced_in_emacs_29/)
- [Emacs Stack Exchange: Which option to use for setting a variable](https://emacs.stackexchange.com/questions/78419/im-unsure-which-option-to-use-for-setting-a-variable-setq-customize-set-variable)

Check out the [official Emacs docs](https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html) on `setopt` as well.

## Closing Thoughts

I always knew that `setq` was flawed, but I kept using it for ages mostly because of inertia.
I didn't like the long name of `customize-set-variable` and I never use the `M-x customize`
directly. I guess that's why I rarely bothered to have setter callbacks in the packages that
I wrote and maintain. Going forward I'll certainly reconsider this.

That's all I have for you today. If you haven't adopted `setopt` already, go wild and `setopt`
all the things!

[^1]: How big of a change? Depends on whether you're using `use-package` and how exactly are you using it! :D (in case you're wondering - `:custom` settings are handled with `customize-set-variable` internally)

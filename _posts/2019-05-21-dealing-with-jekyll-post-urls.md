---
layout: post
title: Dealing with Jekyll Post URLs
date: 2019-05-21 17:52 +0200
tags:
- Jekyll
---

A while ago I wrote about migrating Emacs Redux from [Octopress to Jekyll]({% post_url 2018-11-06-back-in-black %}).
While I'm really happy with Jekyll overall, there has always been one thing that
frustrated me a bit - namely linking to other posts. The syntax for this is the following:

``` markdown
{% raw %}
{% post_url name-of-post %}
{% endraw %}
```

I know this probably doesn't seem like a problem, but posts in Jekyll
are usually prefixed with a timestamp (e.g. `2019-05-21-some-post` )
which makes it really hard to get the name right without consulting
the list of posts first.  Luckily for us it's trivial to write an
Emacs command that helps with this.

``` emacs-lisp
{% raw %}
(defun jekyll-insert-post-url ()
  (interactive)
  (let* ((files (remove "." (mapcar #'file-name-sans-extension (directory-files "."))))
         (selected-file (completing-read "Select article: " files nil t)))
    (insert (format "{%% post_url %s %%}" selected-file))))
{% endraw %}
```

Here's how this command looks like in action:

![jekyll_post_url.gif](/assets/images/jekyll_post_url.gif)

I don't know you, but I'm totally loving this. You can easily extend the core idea for all sorts of similar tasks
that require transforming a bit the files in the current directory. Here's a similar helper
for dealing with image URLs:

``` emacs-lisp
(defun jekyll-insert-image-url ()
  (interactive)
  (let* ((files (directory-files "../assets/images"))
         (selected-file (completing-read "Select image: " files nil t)))
    (insert (format "![%s](/assets/images/%s)" selected-file selected-file))))
```

Let's see this in action:

![jekyll_image_url.gif](/assets/images/jekyll_image_url.gif)

That's all I have for you today! Meta-X forever!

---
layout: post
title: Working with Git Worktrees in Magit
date: 2026-09-02 17:28 +0300
tags:
- Magit
- Git
---

I'll admit that until fairly recently I had no idea git worktrees existed.
They've been part of git for a decade[^1] and I never once needed them. Feature
branches did the job just fine - create a branch, do the work, merge it, delete
it, and start over.

What finally introduced me to worktrees was, of all things, AI coding agents.
Tools like Claude Code create a worktree for each task, so several agents (or
several sessions of the same agent) can work on the same repo in parallel
without stepping on each other - or on you. Suddenly my projects directory was
full of `cider-this` and `cider-that` siblings, and I figured I should
understand what's actually going on there.

<!--more-->

## Worktrees vs Branches

A branch is just a movable pointer to a commit, and making one is basically
free. The catch is that a repository has a single working directory, so working
on two branches means switching that directory back and forth. You know the
routine: stash your half-done work (or commit it), check out the other branch,
do the thing, check out the first branch again, unstash. It works, but it's
tedious, and it gets worse when the two branches leave your project in
different build states and every switch means recompiling half the world.

A worktree gives you an additional working directory attached to the same
repository:

``` shellsession
$ git worktree add ../cider-smart-targeting -b smart-form-targeting
```

Now `~/projects/cider-smart-targeting` is a full checkout of that branch, while
your main checkout stays exactly where it was. The object database, refs,
stashes and remotes are all shared - a worktree is not a clone, so fetching in
one is fetching in all, and creating one is nearly instant. Each worktree gets
its own `HEAD` and index, and git enforces one simple rule: a branch can only
be checked out in one worktree at a time.

When are they worth it? Whenever two things need to happen at the same time: a
long test run on one branch while you work on another, reviewing a PR without
disturbing your half-done work, or - the reason everyone is talking about them
these days - AI agents doing their thing in isolation. The price is pretty
modest: your working files exist on disk more than once, and anything that
isn't tracked by git (dependencies, build caches, `node_modules` and friends)
has to be set up again in each worktree.

If branches have never felt limiting to you, that's fine - they didn't for me
either, for over a decade. Worktrees are one of those features you don't miss
until your workflow changes.

## What About Jujutsu?

While we're on the topic of working copies - the most interesting thing
happening in version control right now is
[Jujutsu](https://jj-vcs.github.io/jj/) (`jj`), a git-compatible VCS that makes
the problem worktrees solve mostly go away. In jj the working copy *is* a
commit, snapshotted automatically as you work. There's no staging area and no
stash, because there's no uncommitted state that could be lost or get in the
way - switching contexts is always safe. It also has proper support for
parallel working directories (`jj workspace`), plus an operation log that makes
practically everything undoable. That last bit is part of why the AI agent
crowd has taken an interest in it too.

You can use jj on top of an existing git repository (they call this a colocated
repo), and your colleagues - and Magit - will keep seeing a normal git repo.
I'm still just an observer here, but it's clearly a project to keep an eye on.

## Worktrees in Magit

Back to Emacs land. Magit has had worktree support for years, hiding behind `Z`:

| Key   | Command                   | Description                                    |
|-------|---------------------------|------------------------------------------------|
| `Z b` | `magit-worktree-checkout` | Check out an existing branch in a new worktree  |
| `Z c` | `magit-worktree-branch`   | Create a new branch and worktree in one go      |
| `Z g` | `magit-worktree-status`   | Jump to another worktree's status buffer        |
| `Z m` | `magit-worktree-move`     | Move a worktree                                 |
| `Z k` | `magit-worktree-delete`   | Delete a worktree                               |

The best part is that there's nothing else to learn. Each worktree gets its own
Magit status buffer, and every Magit command operates on the worktree the
current buffer belongs to. `Z g` is the only switching mechanism you need, and
even that is just a shortcut for visiting another status buffer.

A few practical tips from my (admittedly recent) experience:

- By default the status buffer doesn't list your worktrees. Fix that with:

``` emacs-lisp
(magit-add-section-hook 'magit-status-sections-hook
                        #'magit-insert-worktrees
                        nil t)
```

  Now every status buffer shows all worktrees of the repo, and you can hit
  `RET` on any of them to jump there.

- Create worktrees as *siblings* of the main checkout with descriptive names
  (`cider-smart-targeting` next to `cider`), not nested inside it - nesting
  confuses `grep`, `find` and plenty of other tools.

- Magit's branch selection annotates branches that are checked out in another
  worktree with the worktree's path, and refuses to check them out a second
  time (that's git's rule, not Magit being difficult). If you ever wondered why
  a branch "won't check out", that's usually why.

- Each worktree is its own project as far as `project.el` (or Projectile) is
  concerned, so project switching, per-project buffers and search all work
  naturally.

- Anything that's not tracked by git doesn't come along for the ride. You'll
  have to install dependencies once per worktree and start with a cold build
  cache. For Elisp that costs you nothing; for a big JVM or JS project it's the
  main downside of the whole approach.

- When you're done with a worktree, delete it with `Z k` (or with
  `git worktree remove` from the command line). If you just delete the
  directory by hand, `git worktree prune` will clean up the leftover
  bookkeeping.

## Closing Thoughts

These days my workflow with agents usually looks like this: an agent does its
work in a worktree, I review the changes there in Magit (often while another
task is running in a second worktree), and the worktree goes away once the
branch is merged. Maybe one day I'll find other uses for worktrees - we'll see.

Are you using git worktrees - and did you discover them the same roundabout way
I did? I'd love to hear about it in the comments!

That's all I have for you today. Keep hacking (in parallel)!

[^1]: They were introduced in git 2.5, released all the way back in July 2015.

---
layout: post
title: Deleting Git Branches with Magit
date: 2021-03-23 18:25 +0200
tags:
- Magit
---

How do most people delete local or remote git branches? Well, it's quite
simple actually:

1. Google for "how to delete (remote) git branch"
2. Find the wildly popular [StackOverflow topic on the subject](https://stackoverflow.com/questions/2003505/how-do-i-delete-a-git-branch-locally-and-remotely)
3. Pick one of the options outlined there

Some people would claim that they actually know how to delete git branches, but I'm convinced they are
all liars! Anyways, most people end up doing one of the following:

``` shellsession
# delete a local branch
$ git branch -d branch_name
$ git branch -D branch_name
# delete a remote branch
$ git push -d origin <branch_name>
```

This obviously gets the job done, but we're Emacs users and we have more convenient
options at our disposal. With Magit the process is as simple as:

1. Open the Magit status buffer (`C-x g`)
2. Press `y` to get a listing of all branches and tags in the git repo
3. Navigate to the branch you want to delete and press `k`

That's it! As a bonus you can select multiple branches using a region (press
`C-SPC` and start moving around to select several branches) and you can remove
them all at once!  You'll get prompted to confirm the deletion, so there's
nothing to be afraid of if you want to try this out.  Note, however, that Magit
does not support marking non-consecutive branches (something that `dired` allows
you to do for files and directories).

Here's how this functionality looks:

![magit](/assets/images/magit_branches.png)

Notice that I've selected 4 branches and I can delete them by pressing `k` at this point.

With that newly acquired knowledge you've got no excuse to keep around obsolete branches, so get wild and clean up
those messy git repos of yours! That's all I have for you today! Keep hacking!

---
layout: post
title: Emacs as Your Calendar
date: 2021-06-08 09:23 +0300
tags:
- Calendar
- Utilities
---

Even in the age of smartphones I still prefer to check the calendar on my computer
and I've often been disappointed by the horrible default calendars that ship with
some operating systems (e.g. macOS's calendar accessible via its systray).

Fortunately for us, Emacs users, we always have access to a proper calendar regardless of our
OS and desktop environment (if any) - `M-x calendar`. By default it looks something like this:

![calendar_default.png](/assets/images/calendar_default.png)

Nothing fancy here, just a simple calendar that highlights the current date.
You can easily jump to any date by pressing `o` (it stands for `other`) and
go back to the current date by pressing `.`. You can also use `<` and `>` to
move back and forward in chunks of 3 months (by default).

You might have noticed that the calendar by default assumes that the week starts on Sunday, although in many countries (Bulgaria included) it actually starts on Monday.
You can easily change this:

``` emacs-lisp
(setq calendar-week-start-day 1)
```

Sunday is `0`, Monday is `1` and so on.

You can go a step further and localize the calendar like this:

``` emacs-lisp
;; everything's better in Bulgarian
(setq calendar-day-name-array ["Неделя" "Понеделник" "Вторник" "Сряда" "Четвъртък" "Петък" "Събота"]
      calendar-day-abbrev-array ["Нд" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-day-header-array ["Нд" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Януари" "Февруари" "Март" "Април" "Май"
	                             "Юни" "Юли" "Август" "Септември"
				                 "Октомври" "Ноември" "Декември"])
```

If you already have the calendar open you'll have to do `M-x calendar` or `M-x calender-redraw` to reflect the new settings.

Did you notice that the configuration uses [Emacs Lisp arrays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Arrays.html)? They are so uncommon that I'm pretty sure most people don't even know about their existence.

And here's the final result:

![calendar_bulgarian.png](/assets/images/calendar_bulgarian.png)

There are many more things that you can with the calendar (e.g. configure there national holidays, leverage integrations with `org-mode` and so on), but they are beyond the scope of this short intro. As usual I encourage all of you to share your favorite
tips and tricks related to the calendar in the comments.

I hope you learning something useful today. Keep hacking!

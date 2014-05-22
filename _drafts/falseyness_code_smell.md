---
title: Falsey-ness is a Code Smell
layout: post
tags: coding
---

After a recent code review, my co-workers and I had a rather animated
discussion about the merits of relying on falsey-ness in
code. Specifically, we were debating whether it was best-practice to
rely on the fact that most languages treat certain values as `false`
when evaluating the variable in a boolean context. I was against
relying on this, prefering to check for the specific falsey value you
expect, and pretty much everybody else was for it.

Since we've had this discussion before, and it's difficult to
illustrate all my points in person, I wanted to make my case on my
blog.

## Program in the language of your domain ##

## Principle of least surprise ##

## Avoiding sentinel values ##

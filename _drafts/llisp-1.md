---
title: Writing a Lisp in LLVM IR - Part 1
layout: post
tags: coding
---

For a few years now, I've been working, in fits and starts, on a
little side project: [llisp](https://github.com/spiralman/llisp). It's
a very small lisp implemented entirely in
[LLVM IR](http://llvm.org/docs/LangRef.html). When I say very small, I
mean it: it doesn't have most of the things you might expect a programming
language to have, like numeric types, or a garbage collector, or even
a way to free memory you've allocated. I will show, though, that it
can be use to perform real computation.

The thing that got me started on the project was reading an article by
Peter Norvig titled
[(How to Write a (Lisp) Interpreter (in Python))](http://norvig.com/lispy.html),
in which he builds a small but usable subset of Scheme in 117 lines of
Python. In the end he mentions Alan Kay's
[Early History of Smalltalk](http://gagne.homedns.org/~tgagne/contrib/EarlyHistoryST.html),
specifically the
[assertion](http://gagne.homedns.org/~tgagne/contrib/EarlyHistoryST.html#17)
that "the most powerful language in the world" could be defined in "a
page of code." Peter isn't so sure if his Python implementation
counts, given that it may include the entire Python interpreter, so I
thought: "how hard would it be *without* the interpreter?" Of course,
if I wrote it in some other language, the compiler might count too, so
I thought the safest course of action would be to implement it in
assembly.

## LLVM IR ##

Not wanting to be tied to a particular instruction set, I decided to
use LLVM IR instead of a real ISA. LLVM IR is a language used as an
intermediate between language-specific "frontends" and
architecture-specific "backends", to enable a single compiler to
target different instruction sets, but you can also write code in it
directly. Using it is cheating a little bit, because:

1. It allows you to use an infinite number of registers; this saves a
   lot of rote moving data in and out of memory, or learning the
   obscure addressing modes of whatever processor you're
   using. *cough* x86 *cough*.
1. It has a syntax for defining functions and an instruction for
   calling them; this means you don't have to manually manage the call
   stack (although, modern ISAs often have instructions for
   simplifying this).
1. It lets you define structures and arrays and index into them; this
   saves doing a lot of annoying math and having to keep track of what
   fields go where in the comments.

I also cheated a little bit more and allowed myself access to a few
libc functions: `malloc`, `getchar`, `ungetchar`, `putchar`, `fopen`
and `fdopen`. Aside from `malloc`, these are mostly wrappers around OS
system calls. while the code *backing* those system calls is far from
simple, computing environments when Kay's claim was made were also
very different, and there were a lot fewer layers between your code
and the little bits of iron on the tape.

## Why? ##

I'm giving myself a pass on these things because what I'm really
after is: how simple can a programming language be and still look like
a normal programming language? Is there some concept we can define, or
tool we can build, that can bootstrap something that looks complex out
of something simple? And, having built that, can we prove to ourselves
that the whole thing really *is* simple, because we can hold it all in
our head at once?

## Let's Build It ##

Or course, anybody who's heard of lisp knows that, in lisp, that tool
is:

> Parentheses!

Just kidding! But lisp really does have a single, simple tool that
underlies the whole language: the "cons cell". A "cons cell" is the
fundamental building block of a linked list, and lisp is, after all, a
LISt Processor.

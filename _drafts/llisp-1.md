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
system calls. While the code *backing* those system calls is far from
simple, computing environments when Kay's claim was made were also
very different, and there were a lot fewer layers between your code
and the little bits of iron on the tape. But, maybe that's just my
excuse.

## Why? ##

I'm giving myself a pass on these things because what I'm really
after is: how simple can a programming language be and still look like
a normal programming language? Is there some concept we can define, or
tool we can build, that can bootstrap something that looks complex out
of something simple? And, having built that, can we prove to ourselves
that the whole thing really *is* simple, because we can hold it all in
our head at once?

## Let's Build It ##

Of course, anybody who's heard of lisp knows that, in lisp, that
simple tool that underlies the whole language is:

> Parentheses!

Just kidding! But lisp really does have a single, simple tool that
underlies the whole language: the "cons cell". A "cons cell" is the
fundamental building block of a linked list, and lisp is, after all, a
LISt Processor.

Before we can define our cons cell, though, we need some way to pass
values around the interpreter. Since lisp is dynamically typed, we
need to be able to pass *any* type of value around, but be able to
figure out what the actual type and value are when we need to operate
on them. For llisp, we'll do this with a "boxed value", which means we
put the value in a "box" with a tag on it indicating what type it is.

To do that, we define a struct:

```
%object = type {
        i32,  ; Tag
        i8*   ; Value (may be bitcast, if it safely fits in a pointer)
}
```

Here, the first element of the struct is a 32 bit integer which
encodes the type (`0` is a list, `1` is a token, etc.), and the second
element is a pointer to the actual value. In the case where the actual
value could fit within the size of the pointer (say, an integer),
we'll just store it as if it were a pointer. `i8*` is like `char *` in
C, and we'll use it a lot like `void *`: it's a pointer to an address,
and we're not really saying how much space is allocated to whatever is
there (although, it's at least 8 bits, but, of course, it couldn't be
smaller anyway).

Also, just to ease into things, we'll define a couple functions for
easily getting at the tag and value of a given object:

```
define i32 @tag(%object* %obj) {
       %tagPtr = getelementptr %object* %obj, i32 0, i32 0
       %tag = load i32* %tagPtr
       ret i32 %tag
}

define i8* @unbox(%object* %obj) {
       %valPtr = getelementptr %object* %obj, i32 0, i32 1
       %val = load i8** %valPtr

       ret i8* %val
}
```

Here you can see that an operation which, even in C, would be so
simple as to not even need a function takes a few lines of code in
LLVM IR (and could be even more in a real assembly language).

`getelementptr` takes a pointer to a complex type (an array or struct)
and one or more offsets into that structure, and returns a pointer to
the thing inside. It's a little confusing, because it *looks* like a
nested array lookup (`foobar[0][1]`), but the thing to keep in mind is
that it doesn't actually *do* the lookup. In fact, if all the
parameters passed to it are literals, it shouldn't compile down to any
instructions at all. It's just statically multiplying offsets by
sizes, adding them up, then adding them to the pointer value to give
you a new memory address.

`load` actually loads a value out of memory at a given address and
stores it into a "register" for further instructions to operate on
it. In LLVM IR, you define as many virtual registers as you want, and
give them all names. The backend and optimizer then figure out what
the optimal use is of the actual, finite registers available to the
CPU.

So, there you go, two lines of code to write `object->tag`. This might
end up taking a while...

## The Cons Cell ##

Well, lets plow on through to the cons cell. In order to store one,
we're going to need another struct: this one to store the value in the
cell, and then a pointer to the next value in the list:

```
%list = type {
      %object*,  ; Value (null on last element)
      %object*   ; Next node (null on last element)
}
```

Now, let's see how we create one of these things:

```
define %object* @cons(%object* %head, %object* %tail) {
       %is_null = icmp eq %object* null, %tail
       br i1 %is_null, label %cons_list, label %check_list
```

We're defining a function, called `cons`, that takes two arguments:
a value to put at the front of the list, and the list to add it to. In
lisp, you'd call it like:

```lisp
(cons 1 somelist)
```

The first thing we're doing is checking if the tail is `null` (in the C
sense, not `nil` in the lisp sense). First, we use the `icmp`
function, which compares integers and returns `1` if they are equal,
or `0` if they are not. In this case, we're comparing the tail pointer
against `null`.

Next, we have to do something different if `tail` is or isn't null. IR
doesn't have `if` statements; instead, it just has branches:
basically, conditional `goto` statements. In this case, if `tail` is
`null`, we'll jump ahead to a bit of code labeled `cons_list`,
otherwise, `check_list`.

```
check_list:
       %tailTag = call i32 @tag(%object* %tail)
       %is_list = icmp eq i32 0, %tailTag
       br i1 %is_list, label %cons_list, label %cons_nil
```

Here's `check_list`. It gets the `tag` of the tail object by calling
the `tag` function we defined earlier. Then, it compares it to `0`
(which is the tag for lists). If it is a list, it branches to
`cons_list`, otherwise, it branches to `cons_nil`.

The idea here is to allow the programmer to "cons" two values
together, without having to first cons the second one with `nil`:

```lisp
(cons a b)

; instead of
(cons a (cons b nil))
```

Another interesting thing to note here: in our first null check, we
took one of two branches, but one of them was just the line after the
branch. Why do we have to do this, instead of just conditionally
jumping and allowing execution to "fall through" to the next line if
the condition fails?

The answer lies in the fact that LLVM lets us use an infinite number
of registers: in order for it to properly determine when it can safely
stop keeping a particular "virtual" register in a real register on the
CPU, it needs to be able to check if any code that executes in the
future will need that value. Allowing conditional jumps makes that a
lot harder, so we have to be explicit about both sides of the branch.

Now, with all that checking out of the way, lets *finally* build a
cons cell:

```
cons_list:
       %listSize = getelementptr %list* null, i32 1
       %listSizeI = ptrtoint %list* %listSize to i32

       %listSpace = call i8* @malloc(i32 %listSizeI)
       %listPtr = bitcast i8* %listSpace to %list*

       %valPtr = getelementptr %list* %listPtr, i32 0, i32 0
       store %object* %head, %object** %valPtr

       %nextPtr = getelementptr %list* %listPtr, i32 0, i32 1
       store %object* %tail, %object** %nextPtr

       %objectPtr = call %object* @newObject(i32 0, i8* %listSpace)
       ret %object* %objectPtr
```

So, first we have to calculate how big a list struct is on the machine
we're running on. We do this by calculating the offset of one struct's
width from `null`, which is, conveniently, also `0`, giving us the
size of one struct. We then cast the pointer to an integer using
`ptrtoint`.

Next, we call `malloc` with the size we just calculated, allocating
space for the struct. Since `malloc` returns a `char *`, we have to
cast that to a `list *` to operate on it, using `bitcast`. This is
just like `list * my_list = (list *)malloc(sizeof(list));`.

Next, we calculate the address of the first element of the `list`
struct, and we store our `first` argument there. Then, we do the same
thing with the `tail`. This is the same as:

```c
myList->head = head;
myList->tail = tail;
```

Finally, we call `newObject`, to allocate a new `object` struct. This
is a function that's defined next to `tag` and `value`; I've omitted
it here, but it basically does the same thing we just did: allocate
space for the `object` struct, store the tag and value into that
allocated space and return the address of the struct. Lastly, we
return the object we just created.

Now, if you aren't totally lost, you might be remembering that, way
back at the beginning, we branched to a label called `cons_nil` if the
`tail` value wasn't `null` or another list. Here's what that looks
like:

```
cons_nil:
       %nil = load %object** @val_nil
       %tailList = call %object* @cons(%object* %tail, %object* %nil)
       %fullList = call %object* @cons(%object* %head, %object* %tailList)
       ret %object* %fullList
```

Just like I said, it's just a shortcut for `(cons a (cons b nil))`, so
we can implement it by just calling `cons` twice and returning the
value.

## Whew! ##

So, there you go! You now have a cons cell. Congratulations! There are
actually 3 more functions related to cons cells in llisp: `isNil`,
which returns a flag indicating if the list is `nil`, `first`, which
returns the value at the head of a list, and `rest`, which returns the
list stored in the second element of the cons cell. Traditionally,
these would be `car` and `cdr`, but, since we aren't writing for an
IBM mainfrome, I've decided to just call them `first` and `rest`.

You can see these functions in [list.ll](FIXME!)

Hopefully this gives you a feel for how little we have to work with
when we're operating at the level of (essentially) the CPU's
instruction set, and how seemingly simple operations (even in a
language as "low level" as C) are actually made up of several
instructions.

Don't worry, though! Now that we have our trusty cons cell, things
will get easier. In fact, you've already seen a taste of that: for our
"shortcut" to `cons` two values, we just had to call `cons` twice and
return the result.

Next, when we move into the reader, we'll see how we can build
something even more complicated without getting too lost in the weeds.

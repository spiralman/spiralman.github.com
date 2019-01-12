---
title: Writing a Lisp in LLVM IR - Part 2
layout: post
tags: coding, llisp
---

[Last time]() we got started with the llisp project by writing some
functions to create and manage a cons cell. This time, we'll start
parsing some actual text and turning it into data structures (which is
a fancy way of saying "lots of cons cells").

I'll preface this by saying that the reader algorithm we'll implement
is heavily influenced by the Common Lisp reader algorithm. The
CommonLisp HyperSpec has an excellent
[description of the reader algorithm](http://www.lispworks.com/documentation/lw70/CLHS/Body/02_b.htm),
which I recommend you have handy if you want to follow along.

Another thing I'll say before we get started is that I decided to
represent tokens in llisp as C-style strings (i.e. arrays of
characters with a `0` byte at the end). Some of the code could
probably be simplified by using cons cells to represent the contents
of the string, but that would be extremely space-inefficient (2
pointers, i.e. 16 bytes on x86-64, per character) and would also
require a non-tail-call recursive function (you'll note that the CL
reader algorithm is defined as a procedural loop, in which you
conditionally return back to different steps in the algorithm).

In order to simplify things, strings are stored in pre-allocated
blocks of 64 bytes, and things will go very wrong if you ever try to
write a token more than 63 characters long, since there isn't any
bounds checking, let alone code for re-allocating larger blocks. If
this were a real language, this would probably be something we'd want
to fix. You can see the code for handling strings in
[token.ll](FIXME).

One final note: I'll probably be sparer with the code listings in this
post, to keep it from getting too crazy. You can follow along in
[read.ll](FIXME).

## Reading Tokens ##

The entry to the llisp reader (like most lisp readers) is a function
called `read` (it's the "R" in "REPL"). You call the function once,
and it will return a single object representing the first complete
token available to it on input (in our case, it takes a file pointer
as returned from `fopen`, which may be a file on disk, or stdin).

In a normal lisp, if you gave it a file containing `1`, you'd get back
`1` as a number. If you gave it `foobar` you'd get back `foobar` as a
*token*, or quoted string (if you tried to evaluate `foobar`, you
would get an error, unless you had used `def` to define a value for
`foobar`). If you gave it a complete s-expression, like `(foo 1 (bar
biz baz))`, you'd get back a list, in which the first element was
`foo`, the second `1`, and the third another list of `bar`, `biz`, and
`baz`.

llisp's reader works the same way, except that it doesn't understand
any types other than lists (s-expressions) and tokens. Not very
useful, but it's only 700 some odd lines of assembly!

## Getting started ##

```
read_first:
       %firstChar = call i32 @getc(i8* %input)

       %firstEOF = call i32 @feof(i8* %input)
       %is_not_firstEOF = icmp eq i32 0, %firstEOF
       br i1 %is_not_firstEOF, label %leading_space, label %leading_eof

leading_eof:
       ret %object* null
```

After we load a few constants, the first thing we do, at a label
called `read_first`, is read the first character, using `getc`, check
if we are at the end of the file, using `feof`, and, if we are, retur
`null`. Note: this is not `nil`! The reason being, we need some way to
*stop* reading from a file at the end, but also be able to return
`nil` when we encounter a real `nil`, like somebody types `()`, while
continuing to read the file. Other lisps represent this state using an
object of a special type, usually named `EOF`. This might simplify
some things, but I didn't think of it until I'd finished the thing.

```
leading_space:
       %is_leading_ws = icmp eq i32 %firstChar, %space
       br i1 %is_leading_ws, label %read_first, label %leading_newline

leading_newline:
       %is_leading_nl = icmp eq i32 %firstChar, %newline
       br i1 %is_leading_nl, label %read_first, label %leading_macro_check
```

After we check for `EOF`, we check for leading whitespace. We do this
by checking if the character is a space or a newline, each time going
back to `read_first` if it is, or continuing on if it isn't. This just
slurps up any leading whitespace before we get to any actual code. We
have to treat this differently from whitespace later, because, once
we've started reading a token, whitespace means we should stop reading
and return what we have.

```
leading_macro_check:
       %leadingMacroFnPtrPtr = getelementptr [ 256 x %object* (i8*, i32)* ]* @macro_table, i32 0, i32 %firstChar
       %leadingMacroFnValPtr = bitcast %object* (i8*, i32)** %leadingMacroFnPtrPtr to i8**
       %leadingMacroFnVal = load i8** %leadingMacroFnValPtr
       %is_not_leading_macro = icmp eq i8* null, %leadingMacroFnVal
       br i1 %is_not_leading_macro, label %start_token, label %leading_macro

leading_macro:
       %leadingMacroFnPtr = bitcast i8* %leadingMacroFnVal to %object* (i8*, i32)*
       %leading_macro_res = call %object* %leadingMacroFnPtr(i8* %input, i32 %firstChar)
       ret %object* %leading_macro_res
```

Now that we aren't reading whitespace, we have to check what kind of
thing we *are* reading. The first thing we look for is a "macro
character", i.e. "(" or ")". In fancy lisps, that allow reader macros,
any character could be defined as a macro character, which is why
they're called "macro characters" instead of "parantheses". llisp
isn't fancy enough to allow you to define reader macros as llisp code,
but it does have a way for the interpreter to easily add new ones: a
function pointer table.

When we first initialize the reader, we allocate an array of 128
function pointers; for every macro character, we insert a function
pointer into the array at the index of the character's ascii value.

Then, when we read a character, we check its index in the table; if
it's non-null, we call the function and return its result, otherwise,
we continue on.

```
start_token:
       %token = call %object* @newEmptyToken(i32 64)
       call void @appendChar(%object* %token, i32 %firstChar)

       br label %read_token

read_token:
       %nextChar = call i32 @getc(i8* %input)
```

The next bit of code is pretty tedious, so I'm leaving most of it
out. To start with, though, we allocate space for a token and append
the character we have to it. The gist of the rest is: we read the next
character and check if it is whitespace or another macro character. If
it's whitespace, we just return the token we have. If it's another
macro character we put the character we read back into the input
stream using `ungetc` before we return what we have. We have to use
`ungetc` so that the next call to `read` will see the next macro
character and know what to parse next. This allows us to write code
like `(foo(bar baz))` and have the inner list still be parsed
correctly.

If we don't have either whitespace or a macro character, we append it
to the current token, then jump back to `read_token` and try again.

In all the code paths, `read` only returns one of three things:
`null`, meaning there was nothing before the end of the file, the
return value of a macro character parser, if we started with a macro
character, or the next token from the file.

## Parsing Macro Characters ##

That was all pretty tedious, but now we're getting to the good
part. llisp only has two macro character parsers: `read_list` and
`end_list`, corresponding to `(` and `)`, respectively.

We'll start with `read_list`:

```
define %object* @read_list(i8* %input, i32 %char) {
       %nextHead = call %object* @read(i8* %input)
       %is_end = icmp eq %object* %nextHead, null
       br i1 %is_end, label %at_end, label %read_tail
at_end:
       %nil = load %object** @val_nil
       ret %object* %nil

read_tail:
       %nextTail = call %object* @read_list(i8* %input, i32 %char)
       %list = call %object* @cons(%object* %nextHead, %object* %nextTail)

       ret %object* %list
}
```

It takes the input file and the character that was read as inputs. The
character that was read is just to maintain some semblance of
compatibility with Common Lisp's reader macros; we always know it's a
`(`.

First, it calls `read` (recursively, since `read` called `read_list`)
and checks if it got a `null`. If it did, we are already at the end of
a file, so we return `nil`. While it looks like this should be an
error, when we see `end_list`, we will see why it is not always.

Assuming we didn't get back `null`, the return value of `read` becomes
the `head` of the list, and we recursively call `read_list` again, to
read the rest of the list. Then, we `cons` the first token with the
list returned by `read_list` and return that list.

`end_list` is very simple:

```
define %object* @end_list(i8* %input, i32 %char) {
       ret %object* null
}
```

It just returns `null`. To see why, lets walk through parsing a list:
`(foo bar baz)`.

1. We call `read`, and the first character is a macro character, so it
calls `read_list` (based on the function pointer table).
    1. `read_list` calls read, and gets back `foo`, which is non-nil, so
      it calls `read_list` again.
        1. That `read_list` calls `read` again, gets `bar`, calls
          `read_list`.
            1. That `read_list` calls `read`, gets `baz`, calls `read_list`
                1. That `read_list` calls `read`, which calls `end_list`
                  (there's a `)` next), which returns `null`.
                1. So the innermost call returns `nil`
            1. The next call out conses that after `baz`: `(cons baz nil)`
        1. The next call out conses that after `bar`: `(cons bar (cons
          baz nil)`
    1. The outermost `read_list` call conses that after `foo`: `(cons
      foo (cons bar (cons baz nil)))`
    1. The full list `(foo bar baz)` gets returned by the outermost
      `read_list`.
1. `read` returns the value from `read_list`, so we have successfully
  read the list `(foo bar baz)`.

You'll note that, since we aren't distinguishing between the end of a
file and the end of a list, we can't tell when there are unbalanced
parentheses. If we wrote `(foo bar baz`, we would get the same result:
the last call to `read` would return `null` because it got an `EOF`
(not because it called `end_list`), but the innermost `read_list`
can't tell the difference, so carries on its merry way.

## And That's the Reader ##

Aside from the function to initialize the macro character function
pointer table (which you could do statically, if you weren't lazy like
me), that's the entirety of the llisp reader. I think it speaks to the
simplicity of lisp's s-expressions that the entire parser (minus some
string manipulation code) clocks in at 98 non-blank, non-comment lines
of assembly code!

It also speaks to the power of the cons cell, and notion of recursive
function calls, that the entire language can be parsed with just three
mostly simple functions. Most of the code complexity is in `read`,
where it has to repeatedly check for whitespace and macro characters
in a drawn-out, procedural way. The real "meat" of the algorithm is
really in `read_list`, which is only about 10 lines long!

Next, we'll turn this into a real language by implementing an
evaluator, and see how a simple linked list can be used to represent
an entire program.

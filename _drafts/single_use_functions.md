---
title: Why I Like Single Use Functions
layout: post
tags: coding
---

Over the years I've met more than a few people who dislike "single-use
functions." The typical arguments are that they are inefficent and
make the code more difficult to read, and some of these people will
even call them out on a code review. However, I will often write
single-use functions, and here's why.

First, it must be acknowledge that function calls *do* add overhead,
especially in a lot of interpreted languages, so it's difficult to
outright deny the efficiency argument. The only thing I can say to
that is, "premature optimization is the root of all evil." As long as
performance is acceptable to your users, I would rather have slightly
slower, but more maintainable, code any day of the week. I bet your
users would rather you be adding new features and fixing bugs, too, as
long as you keep things "fast enough."

The simplest reason to write single use functions, and also the
easiest to rebuke, is that they make code easier to read. If you take
a complicated procedure and break it up into higher-order operations,
it will be easier for somebody skimming the code to get the gist of
what the function does:

```python
def activate_crm114(code):
    enter_code(code)

    test_self_destruct()

    enter_all_receiver_circuits()
```

If you don't care about the details, this code is pretty
straight-forward. The argument *against* this is that, if you *do*
care about the details, it's harder to know exactly what is going
on. I certainly agree with that: if I'm reading through the code,
trying to trace down a bug, I'm not going to take those functions'
names at face value (there's a bug *somewhere* after all, it could be
in one of them). Having to jump around the file to see what happens
inside them certainly can slow things down.

*However*, there's another advantage, and it directly affects
readability and debugability: encapsulation. This is an idea I picked
up from a
[blog post](http://michaelfeathers.typepad.com/michael_feathers_blog/2013/11/unconditional-programming.html)
by Michael Feathers, although he doesn't explicitly talk about it in
terms of single-use functions. The idea, as he states it, is that the
various blocks of conditional statements can be coupled to each other
via shared variables. He's arguing for reducing the conditionals in
your code (definitely a good idea), but it also points to an advantage
of single-use functions.

Inside the function above, I *know* that the only function which uses
the `code` variable is `enter_code`, and the interpreter is going to
enforce that for me. If I'm looking for a bug that I know involves the
use of that variable, I can safely ignore `test_self_destruct` and
`enter_all_receiver_circuits` because they can't possibly access that
variable. The use of the variable has been *encapsulated* into
`enter_code` and `activate_crm114`.

This also makes it easier to change the interface to `code` because I
know that I won't affect the logic in either of the functions which
don't take it as a parameter. If all of that logic was inline to
`activate_crm114` I would have to carefully read through it to find
and fix any code I might break while changing interfaces.

Finally, single-use functions can force a code-smell when your logic
starts breaking encapsulation. Let's say that the logic to the
functions was all inline to the outer function, and the various bits
of logic were growing more complicated, and starting to access a lot
of shared variables. At a certain point, it will be obvious that
*something* is wrong: the function will get hard to understand, hard
to add to, and hard to fix when it's wrong. It would be harder,
though, to see exactly where all the "spaghetti" was getting tied
together.

If the functions are broken out, as they are above, and they start
operating on the same variables, then you'll have to pass those
variables around to the individual functions:

```python
def activate_crm114(code):
    interface = PanelInterface()
	
    activation_token = enter_code(interface, code)

    test_result = test_self_destruct(interface, activation_token)

    enter_all_receiver_circuits(interface, activation_token, test_result)
```

Having to pass all those variables around can start to feel very
cumbersome very fast, so it's easier to see when you need to start
refactoring things. You might not think about referencing a variable
further down inside a function, but you'll probably think about adding
a new parameter to the function, passing it at the call site, and
updating all the unit tests to pass the new parameter in.

Making the parameters explicit also makes it obvious *which* concerns
are mixed up, and where. If an instance of `PanelInterface` is needed
for all the functions, maybe they should be members of that object?
`activation_token` and `test_result` being passed around imply that
maybe some other object is needed to help maintain state, or maybe
`enter_all_receiver_circuits` is doing some work that would be better
done somewhere inside `test_self_destruct`.

Functions are often taught as providing code reuse by allowing code to
be invoked from several different places. However, it's important to
remember that they also provide other features, which can be just as
valuable: scoping provides encapsulation, parameters document
dependencies, and the entry point provides a place to perform unit
testing.

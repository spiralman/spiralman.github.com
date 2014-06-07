---
title: The Limitations of the REPL
layout: post
tags: coding
---

I've been reading a lot about Clojure recently, working on a little
side-project in it and, eventually, ClojureScript for the
frontend. One thing I see a lot of, and this goes way back to the Lisp
community in general, is a workflow that is heavily dependent on the
REPL or live-coding tools, like Emacs and LightTable.

What triggered me to write this post was reading through the Om
[tutorials](https://github.com/swannodette/om/wiki/Basic-Tutorial),
which is written assuming you're developing inside LightTable, a text
editor built around the concept of "live coding." But then, before I
could write the post, Apple announced
[Swift](https://developer.apple.com/swift/), which also features,
prominently, a live coding environment.

Now, the REPL and, especially, live coding environments are definitely
cool features to have, and many an intro-to-language-X tutorial has
been made simple by the presence of a REPL for the language. Nothing
says "we're simpler than C++!" than letting people type code in, hit
enter, and see the results immediately; no compilers, no linkers, no
Makefiles! In fact, even
[C++](http://root.cern.ch/drupal/content/cling) is simpler than
[C++](http://www.artificialworlds.net/wiki/IGCC/IGCC).

For me, though, the Lisp world seems to go a little overboard for live
coding. In addition to the Om tutorial above, there's also
[Overtone](http://overtone.github.io/), which doesn't even document
how to import their library except via the `overtone.live` namespace,
and [Ring](https://github.com/ring-clojure/ring), whose
[Getting Started](https://github.com/ring-clojure/ring/wiki/Getting-Started)
guide tells you how to start a new server in the REPL, but not how to
actually start the server process directly. Yes, it's cool that you
can run a server from the REPL, but I hope you're not going to deploy
your application by manually running the REPL on a server somewhere.

The manual part in all of these workflows is the thing that bothers me
the most. The Om tutorial is filled with instructions like "type this
function in, but don't evaluate it yet" (because its definitions
depends on functions you haven't written yet). That's something I
shouldn't have to care about! If I typed out the program in a text
file, and ran it separately, the definitions would be in the right
order because I put them in the right order. If I change a function,
the definitions are still in the right order when I run the file
again, because I didn't re-order anything.

The REPL, and live coding, often make the user responsible for
evaluation order, and most programming languages are sensitive to the
order that statements are evaluated in, causing errors if the order is
wrong. This means that you're introducing the chance for user error
into the running of your program, when you should be *reducing* the
chances of user error.

It also means that execution isn't reproduceable, because the steps
you've used to execute your program are in your head, not written down
as code. Reproduceability is a fundamental concept in good
engineering. If you can't reproduce a process, you can't trust that
the process will work again. Live coding environments solve this
problem a little bit, because you're executing, live, a saved file
somewhere. But they often have features (like execute just this
expression), which subvert that reproduceability.

At work, we run into this wall often. We have a little script, called
`shell.py`, which is available on all the application servers. We can
SSH into the servers, import the script on the Python REPL, and the
script takes care of loading the application configuration, connecting
the ORM to the correct database, etc. It's very useful for poking
around and debugging issues because you can easily inspect the state
of the database and start inferring where the code went wrong.

But it's also dangerous to our process because it encourages people to
*fix* things in it. Once you've figured out the problem, there's a
temptation to just fix the record and write it back out. The problem
with that, though, is if the error happens again, you have to re-type
the fix into the REPL. And, if somebody else is debugging the issue,
they have to figure out how to fix the problem all over again.

For this reason, we discourage people from fixing things this
way. Even though it takes longer, it's better to write a tool, unit
test it, have it code reviewed, and check it into version
control. That way, if the process needs to be repeated, we know we can
do it again. It's easy to think you won't ever have to do something
again, but you're almost always wrong.

This is also one of the things which bother me about IDEs and
interactive debuggers. They make certain, pre-conceived operations
very easy and reproduceable (you just click this button, we we'll
build your code), but they make it very difficult to compose
higher-order operations in a reproduceable way. If you want to build
your code, run the tests, deploy to stage if they passed, run
integration tests, and then deploy to production if those passed, good
luck finding a button in your IDE that can do that for your particular
environment.

Getting back to Lisp again, I don't believe that the Ring developers,
for example, actually think you should be deploying your application
by manually running a REPL on a server somewhere. Instead, they're
trying to lower the barrier to entry for people new to their
environment. I think this is a noble goal, and probably reflects the
academic roots of Lisp, but I also think it does a disservice to the
people you're teaching.

People aren't learning these languages and tools just because they are
there. They're learning them because they want to use them to solve
problems, and responsible engineering means solving problems in
reproduceable (and, thus, less error prone) ways. This is an attitude
that should be core to how software developers solve problems, and
skipping it at the beginning of the learning process trivializes
it.

As much as I like just getting things done, I feel best when I know
I've done something the right way, and feel confident that it won't
break in the future. We should be building tools that make it easy for
us to create solutions we trust, without burdening us with rote and
manual steps that the computer was designed to do for us.

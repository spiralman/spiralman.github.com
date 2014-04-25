---
title: Designing for Refactoring
layout: post
tags: coding
---

When I was in college, I took a few classes in Software Engineering
(it was originally going to be my major), where they taught us,
mostly, about software design and architecture. All the things they
taught us, all the design patterns and object oriented principles,
were in service of one holy grail: *extensibility*.

The idea of extensibility sounds pretty cool: if you design your
software right, in 5 years, you can come back to it and, _without
modifying any existing code_, you can add some new feature, or support
some new format, that nobody had ever dreamed of when you first wrote
the code. Sounds pretty neat, right?

Of course, the reality is that you can't add *any* feature without
changing existing code; the design patterns are all about providing
hooks for extensibility in particular parts of the code. For example,
let's say I'm writing a bit of code for parsing files. I might write
something a bit like this:

{% highlight python %}
def parse_file(filename):
    with open(filename) as file:
        extension = os.path.splitext(filename)[:-1]
        parser = ParserFactory.build_parser(extension)

        document = parser.parse(file)

        return document
{% endhighlight %}

This code makes it very easy to add new file formats, keyed off of the
file extension. You just register the new parser, with its extension,
implement `parse`, and you're done. You don't even need to modify
`parse_file`.

Now let's say a new requirement comes down from above: instead of
parsing different file formats, users want to be able to parse files
from Dropbox, or other cloud storage providers. Now my factory won't
help me, and I'll have to change `parse_file` again!

But, when you think about it, that's not really a big deal. I only
have to change a couple lines of code, implement another factory for
accessing file contents as a stream, add some unit tests I would have
had to write either way, and push out my changes.

This example is a bit synthesized, but you can probably imagine
(because you've probably seen it before) a system with *lots* of
factories and polymorphism and branching logic. And you can probably
imagine (or remember) how hard it is to reason about a system like
that, and how it's even harder to change it safely. This becomes
especially frustrating when all the complexity is in service of
flexibilty that you found out you never even needed.

Lately, I've been thinking about this problem in a new way: instead of
designing for extensibilty, I've been designing for
refactorability. What this means is, when I implement something that
I think I might need to change, I won't actually add the factory or
class hierarchy, but I will think about how I *would* add it later.

I plan for the change by making sure that adding the logic won't be
painful in the future, even if I still do need to modify existing
code. I try to make sure that the change will only be in one place,
instead of many, and that it won't invole much, if any, usage of the
find-replace feature.

The cool thing about this is that code that's easy to refactor is also
just good code:

* Doing a complicated find-replace means you probably repeated some
  logic you should have put in a function.
* Changing lots of code because a function changed means you probably
  didn't encapsulate your logic correctly.
* Updating lots of mocks in your unit tests means you're probably
  mocking out too many interfaces.
* And not *trusting* your refactoring means you probably don't have
  enough, or the right, unit tests.

What we often forget is that we *always* have to change the code we
write: maybe there's a feature we didn't anticipate, or maybe there's
just a bug we have to fix, but it's rare that some piece of code, in
an actively developed part of the system, never has to be changed. And
that's OK. All good developers accept that subconciously, yet we still
seem intent on avoiding it for features we feel certain we're going to
have to write just around the corner.

My answer has become: YAGNI. You ain't gonna need it. But if you prove
me wrong, I'd be happy to refactor my code to support it.

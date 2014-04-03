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

Now, when I have some new file format, I just need to register the
extension of my format with ParserFactory, and provide a `parse`
method, which returns some pre-defined document type. I never have to
modify `parse_file` again!

Well, not quite. The problem is, I wrote that code in 2005, before
that whole "cloud computing" thing happened, so it's just dealing with
boring old files. Let's say I want to add a feature to parse a file
from Dropbox or Google Drive?

Turns out that the `Parser` interface is probably fine: hopefully
whatever libraries I'm using can return a file-like object (a pretty
common construct in almost any language's standard library), or I can
use a string/file-object wrapper (again, a pretty common construct). I
can also probably get away with not changing the external interface to
`parse_file`, and just pass in URIs with made-up schemes, like
`dropbox://some/dropbox/file`, to indicate I want to load the file
from Dropbox.

That `open` call might be a problem, though. Python's built-in open
function certainly can't read files from Dropbox. I suppose I could
download the file from Dropbox, store it locally, parse it, and (if my
application allows changes), sync it back up at some point, but that's
a pretty cludgey workflow, and has all sorts of extra corner cases. It
would be especially problematic if I'm calling `parse_file` from a lot
of different places, all of which can parse files from cloud
providers.

It turns out that, if I want to implement this feature, I *do* have to
change `parse_file` after all. And, when you think about it, that's
not really a big deal. I only have to change a couple lines of code,
implement another factory for the URI scheme handling (very simple in
most modern languages), add some unit tests I would have had to write
either way, and push out my changes.

Now, let's consider this: in all those years between when I wrote the
code, and I needed to add Dropbox support, I *never* had to support a
new file format. All that file parsing factory code was a waste:
there's only ever one file type, and the requirement that changed was
actually where the file came from, not how it was formatted.

Of course, this example is hypothetical (I'm pretty sure Python didn't
even have context managers in 2005!), but this is something I see all
over the place, and something I've probably done a million times
myself. The fact of the matter is, you may *think* you know where to
put the extensibility points, but you are probably going to be wrong
half the time. In the meantime, all that "extensible" code is more
complicated, harder to understand, and has more points of failure than
simply calling the one function to parse the one file format you
support right now.

What we often forget is that we *always* have to change the code we
write: maybe there's a feature we didn't anticipate, or maybe there's
just a bug we have to fix, but it's rare that some piece of code, in
an actively developed part of the system, never has to be changed. And
that's OK. All good developers accept that subconciously, yet we still
seem intent on avoiding it for features we feel certain we're going to
have to write just around the corner.

The scarier thing to think about is this: lets say that bit of code up
there wasn't in a function, but those few lines were sprinkled
throughout the application wherever a file needed to be parsed. Now
adding Dropbox support really *is* a problem, because we have to find
all the instances of that code and replace the `open` calls with our
URI parsing code. It's definitely a good that to have that logic
encapsulated in a function we can easily refactor, but if you go into
the design thinking that parsing different file types is the point of
extension, you might not really think to encapsulate that logic quite
that way.

I've started thinking about this as "designing for refactoring." Where
I'm working now, we're try to be as iterative in our development
process as we can, and we're getting better at breaking features up
into sprint-size chunks of work. What this means is that I'll often go
into some feature development knowing (or thinking) that, next month,
we're going to need to change this feature in a certain way. However,
instead of building in the extensibility code right there (using a
factory or a higher-order function), I'll just think "how can I write
this so it's simple to refactor it to be more extensible later?"

This doesn't really solve the problem of unanticipated features: I
probably didn't ask myself that question for some change I didn't know
is coming, but it *does* mean that if that next feature I was certain
we'd need never really is needed, I haven't cluttered up the code with
a bunch of logic that only ever takes one path.

Code that is easy to refactor is just generally good code: repeated
logic is replaced with a single function; functions and classes
encapsulate their data and adhere to the single responsibility
principle. None of these concepts claim to let you add a feature
without modifying code, but they do make the code simpler and easier
to read, and they also make it easier to refactor when you finally
find out what the next feature really is going to be.

---
title: Why don't I like C#?
layout: post
tags: coding
---

I spent about the last 3 years at my previous job working almost
exclusively on a large C# application. While I was there, if you had
asked me what I thought of C#, I would have said I hated it. Now, with
a little over a year separating me from that experience, I've upgraded
my opinion to the milder "I don't care for it."

For whatever reason, I've recently been thinking about C#, wondering
what it was, exactly, that I didn't like about it, and wondering what
that says about me, or the language.

Here are the things that I remember bothering me about C# at the time,
and how I feel now. Keep in mind that my experiences were limited to a
couple projects in the same organization. They are certainly coloured
not just by how we used the language there, but by all the other
administrative, political and technical decisions that we made on the
projects.

## Verbosity ##

This certainly felt annoying at the time, but, in retrospect, is
something that really isn't that bad. Compared to Java, C# is like a
language designed for code golf: first-class functions, lambdas and
object literal initializers remove a lot of the boilerplate that Java
requires. C# also doesn't feel any better or worse than C++, which I
enjoy writing.

I think part of the issue might have just been that I was mostly
working in a large, old codebase, which was not a very productive
development environment. Adding the verbosity of a statically typed,
strict-ish OO language made development feel like treading through
molases.

## Language Features ##

This is definitely something that nobody can really complain about,
because C# has all of them. Microsoft loves to practice what I call
"checkbox engineering," presumably so that their marketing departments
write websites like
[this](http://www.visualstudio.com/products/compare-visual-studio-products-vs). As
a result, C# has pretty much every language feature anybody ever
thought of: OO, properties, mixins, first-class functions, lambdas,
closures, async co-routines, generics, dynamic typing, static
typing. They even added SQL-like syntax
[to the language itself](http://en.wikipedia.org/wiki/Language_Integrated_Query#Language_extensions)!

I think the problem here lies, partially, in the fact that many of
these features really do feel like they were added to mark off a
check-box, and the fact that .NET developers treat them that way. When
a new feature comes out there are a lot of blog posts about how
awesome it is, and everybody rushes to use them, but the overall
structure of the applications is still a monolithic, strongly OO,
"enterprise architecture" with inconsistent use of the new features.

## Sealed Classes ##

Only a large, enterprise organization could produce a feature like
this: where you can actually mark a class as "sealed" and prevent any
other classes from subclassing it. But, only an idiot would declare
classes in the standard database library as sealed, without
implementing any meaningful interfaces.

This makes it extermely burdensome to write unit tests for your
database-interfacing code. For example, `DataTable` (which represents,
among other things, the results of a query) has a `Rows` property, of
type `DataRowCollection`. Normally you might want to provide a mock
set of rows, but `DataRowCollection` is both sealed (so you can't
inherit from it) and has no public constructors, so you can't create
an instance of it with your own data.

The only options you have are: actually query data from a database, or
write your own wrappers, with interfaces, around all the methods you
want to call, proxy them to an underlying instance of the core
objects, and write all your code against these wrapper classes.

This might be a little nit-picky, but I think it illustrates the
attitude the language and library designers had, at least early on in
the development of the language. There was an obvious desire to
prevent developers from doing things the designers felt was untoward,
but that also means they are preventing developers from doing useful
things with the language.

## Visual Studio ##

Maybe this isn't quite fair, since it's not actually part of the
language, but it's so pervasive in the C# community that it might as
well be. There are some great articles about how IDEs can
[rot your mind](http://charlespetzold.com/etc/DoesVisualStudioRotTheMind.html),
so I don't want to re-hash all that, but I did have a lot of terrible
experiences with Visual Studio, and am very glad to not be using it
anymore.

First and foremost, Visual Studio is an enormous, slow,
memory-gobbling beast. Since we developed against a trunk with
maintenance branches, I would often have to rapidly switch between
writing a feature in trunk, fixing a bug in one or more maintenance
branches, and sometimes manually merging the fix back to trunk. This
meant I either had to constantly be re-opening the solution file for
the given branch or leave all the instances running all the
time. Since it could take minutes to open a large solution, and each
running instance took up hundreds of MB of RAM, this was a damned if
you do, damned if you don't kind of situation.

(Yes, I know VS afficionados will tell you how much faster it is than
Eclipse. Talk about faint praise.)

The other problem with Visual Studio is what it does to your code:
there's no need to organize anything if can just "go to definition;"
there's no need to keep classes small if autocomplete can find the 1
method in 100 you were looking for; it's better to keep all the
projects in one monolithic "Solution File" than try to run multiple
instances for multiple independent components.

I'd also like to pick one more little nit: build configuration
management. When I'm defining my configuration, I want to build up a
set of values for all build types, and then override specific ones for
individual build types. If I change a default value, I want it to
change for all builds. If I want to change a value for just one build
type, I want all other build types to get the default. This is very
easy to do with an imperative (or even functional) build system:

{% highlight make %}
CFLAGS=-Wall

ifeq ($(BUILD_TYPE), "DEBUG")
    CFLAGS=$(CFLAGS) -O0
endif
{% endhighlight %}

This is almost impossible to do with Visual Studio's build
configuration management. You can define default values once, and they
will get copied when you make a new configuration, but changing them
in the initial configuration won't change them in the copies. I'm told
there's some complex feature you can enable that lets you accomplish
this in their GUI, but if they'd just give you make, or something like
it, it wouldn't be complex at all. It would be just like writing code!

## Community ##

As I said at the beginning, this one is hard for me to judge, because
my experience was as much about the team I worked on as the C#
community at large. I also think that tools like Mono, NuGet and
Microsoft's semi-open-source initiatives have been improving this
situation a lot over the last few years.

That being said, writing in C# almost always meant writing a large,
monolithic application against a SQL database, running on
Windows. Microsoft's documentation almost invariable includes an
example of binding whatever language feature they are talking about to
a `DataTable` from a SQL query. Compilation is done through a GUI,
configuration is done through a GUI, deployment is done through a GUI,
everything needs to be a GUI application that you can click, and
automation is an afterthought that some other team has to worry about.

When it comes down to it, this just isn't the kind of software I want
to build. I'm certain I could use C# to build software the way I want:
using small, independent components, automating the development
workflow, maybe even working entirely from the command line. But, the
fact of the matter is, I don't want to have to fight the tooling to
build things the right way, which is fundamentally what I found so
frustrating about writing C#.

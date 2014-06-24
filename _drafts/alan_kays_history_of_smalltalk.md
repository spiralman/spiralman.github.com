---
title: Alan Kay's Early History of Smalltalk
layout: post
tags: coding
---

http://worrydream.com/EarlyHistoryOfSmalltalk/

> New ideas go through stages of acceptance, both from within and
> without. From within, the sequence moves from "barely seeing" a
> pattern several times, then noting it but not perceiving its "cosmic"
> significance, then using it operationally in several areas, then comes
> a "grand rotation" in which the pattern becomes the center of a new
> way of thinking, and finally, it turns into the same kind of
> inflexible religion that it originally broke away from.

Certainly OOP has run this course, perhaps even already had by 1993,
but we are only recently starting to see the backlash against it
(mostly in terms of the return of functional languages). What have we
learned, and what mistakes are we still making?

> [Programming Languages] all seem to be either an "agglutination of
> features" or a "crystallization of style." COBOL, PL/1, Ada, etc.,
> belong to the first kind; LISP, APL— and Smalltalk—are the second
> kind. It is probably not an accident that the agglutinative languages
> all seem to have been instigated by committees, and the
> crystallization languages by a single person.

How do Ruby and Python fit into this? They were initially designed by
one person, but are now maintained by committee. At their core is
often a kernel of "crystallized style," but they are also an
"agglutination of features."

> In computer terms, Smalltalk is a recursion on the notion of
> computer itself. Instead of dividing "computer stuff" into things
> each less strong than the whole—like data structures, procedures,
> and functions which are the usual paraphernalia of programming
> languages—each Smalltalk object is a recursion on the entire
> possibilities of the computer.

This is a confirmation of Michael Feathers's post on
[Microservices](https://michaelfeathers.silvrback.com/microservices-and-the-failure-of-encapsulaton).

> an OOPL merely focuses the designer's mind in a particular fruitful
> direction. However, doing encapsulation right is a commitment not just
> to abstraction of state, but to eliminate state oriented metaphors
> from programming.

Tell, don't ask.

> It isn't enough to just learn to read and write. There is also a
> literature that renders ideas. Language is used to read and write
> about them, but at some point the organization of ideas starts to
> dominate mere language abilities. And it helps greatly to have some
> powerful ideas under one's belt to better acquire more powerful
> ideas.

?

> The "trick," and I think that this is what liberal arts educations is
> supposed to be about, is to get fluent and deep while building
> relationships with other fluent deep knowledge. Our society has
> lowered its aims so far that it is happy with "increases in scores"
> without daring to inquire whether any important threshold has been
> crossed.

If only we could learn this, and teach this way.

> In the 1990's there will be millions of personal computers. They will
> be the size of notebooks of today, have high-resolution flat-screen
> reflective displays, weigh less than ten pounds, have ten to twenty
> times the computing and storage capacity of an Alto. Let's call them
> Dynabooks.
>
> The purchase price will be about that of a color television set of the
> era, although most of the machines will be given away by manufacturers
> who will be marketing the content rather than the container of
> personal computing.

Pretty amazing for the mid-70s. Video game consoles were probably the
first subsidized devices, but phones are subsidized by data plans, and
the Kindle Fire (and e-readers) are subsidized by content.

> One way to think about progress in software is that a lot of it has
> been about finding ways to late-bind, then waging campaigns to
> convince manufacturers to build the ideas into hardware. ... Most
> machines still have no support for dynamic allocation and garbage
> collection and so forth. In short, most hardware designs today are
> just re-optimizations of moribund architectures.

Perhaps hardware has gotten "fast enough" that we just don't care
anymore. Power and battery usage has become more important than raw
processing power, and reams of code have been written to support
dynamically scaling computational resources in the rare cases where
more processing is needed.

> This higher computational finesse will be needed as the next paradigm
> shift—that of pervasive networking—takes place over the next five
> years. Objects will gradually become active agents and will travel the
> networks in search of useful information and tools for their
> managers. Objects brought back into a computational environment from
> halfway around the world will not be able to configure themselves by
> direct protocol matching as do objects today. Instead, the objects
> will carry much more information about themselves in a form that
> permits inferential docking. Some of the ongoing work in specification
> can be turned to this task

Unfortunately, networks, and the remote systems that talk to each
other over networks, never got reliable enough for this to
work. Instead, things like REST focus on making the network request
obvious, so it can be programmed around. REST is also explicitly about
state, and does not "eliminate state oriented metaphors from
programming." Is there a solution to distributed computing that does,
and is still reliable in the face of unreliable networks and hardware?

---
title: Blog Redesign
layout: post
tags: coding
---

I've decided to try starting up my blog again, and, to mark the
occasion, I thought I'd redo the design. If you remember what the old
one looked like, you'll see that the layout is pretty much the same,
but I wanted to throw out Twitter Bootstrap and design the look and
feel on my own.

Partially this was just to give the site a more unique look, but I
also wanted to try really designing something from scratch. I've done
bits and pieces of CSS at work, but never really had a chance to start
from a concept in my head and proceed to a finished product. I also
wanted to get more familiar with SASS and CSS3 animations.

My main goals were to design something simple and flat while still
looking polished, and to make the design responsive to tablets and
phones, which is where I do most of my blog reading these days. I
didn't want to have a lot of superflous animation, but I did want
transitions to feel smooth and natural. One of the hardest things for
me, as a non-designer, is to pair things down without having the end
result look barren, and I wanted to focus on improving that.

One thing I did not do was start with a drawing or mockup, and I
regret that. I think it would be useful to spend time just getting the
visual design I want out of my head, without worrying about semantic
correctness or maintainability. I'd like to try out Sketch for that,
but haven't been able to justify the $70 for something I'd rarely use.

I've used SASS before, and I always feel let down by the end result:
in my head is a perfect, organized codebase where every color and
length is defined in a variable and re-used, but the actual result is
a mess, with lots of padding and margins specified as one-off lengths
to get everything to line up correctly. Maybe part of the problem is
not doing a detailed drawing ahead of time, or maybe it's the fact
that I don't have the box model memorized, but I think HTML and CSS
really fall short when it comes to expressing the design decisions I
want to make.

The thing I want to express most, but can't, is the positioning of
elements relative to each other. I want to say things like "element X
should be vertically aligned with element Y," or, alternately, "X and
Y should be aligned to a pre-defined vertical rule." There's a certain
amount of this that you can do implicitly, if the elements are
structured correctly in the DOM tree, but you have to have a good
understanding of the browser's layout engine to really pull it off.

Things get especially complicated if you want to support responsive
mobile designs in the same page. Because correct positioning depends
on the hierarchy and order of elements in the HTML, and you might want
to draw elements in totally different places on a tiny phone screen
than a desktop, it's often a struggle to pick exactly how to structure
the HTML so you can easily position an element in both places, using
just a media query. It certainly makes you understand why some
developers just fall back to separate mobile and desktop sites.

I probably need to devote a separate post to the inadequecy of CSS3
animations, but, suffice it to say, I gave up on my original vision
for how some of the animations where going to work. In the end, using
the simpler transition attributes was pretty painless, but they still
have some odd shortcomings that result in having to define the layout
differently than I would have otherwise to make the animations work
well.

The whole effort was certainly a worthwhile experience: I ended up
with a design I liked, and feel more confident that I can build
something halfway decent on my own if I need or want to. Let me know
what you think, and how it works on different devices and browsers.

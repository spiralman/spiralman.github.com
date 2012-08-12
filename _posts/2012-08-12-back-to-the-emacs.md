---
title: Back to the Emacs
layout: post
tags: coding
---
Since I first started using Linux, my editor of choice has been
Emacs. What first drew me to it was my ability to just start typing
into it (unlike, ahem, some other text editors), and what kept me with
it was the fact that I was essentially using a _programming language_
to edit text.

Over the last few years, though, I drifted away. At work, I started
working on an exclusively-Windows product, and at home I bought a
Mac. One thing about Emacs is that it is, first and foremost, a
console application; yes, it does have X11, Windows and Mac UIs, but
the integration into those environments doesn't always feel complete
(for example, the editor's and the OS's copy/paste buffer isn't always
the same). When you spend a lot of time editing files, your text
editor needs to be seamlessly integrated into everything else you do,
and Emacs didn't always feel like it was.

Of course, the elephant in the room when it comes to Windows
development is Visual Studio. If you're going to use Microsoft's
toolchain, you're going to use Visual Studio and its text editor. 

In the last couple versions, Microsoft has re-implemented the internal
build system as MSBuild (their version of Ant). This makes it much
easier to do builds from the command-line, but project management is
still a nightmare outside of the IDE. If you want to add a new source
file to your project, you have to edit a "project" file, which uses a
very verbose XML schema; if you want to add a new project to the
"solution" (think recursive Make), then you have to edit the solution
file, which is implemented in some weird custom syntax and requires
256-bit GUIDs to be generated and assigned to each project.

The thing about Visual Studio is that I hate it. And every week, it
does at least one stupid thing that makes me hate it more. After 5
years of using it, first for the Windows half of cross-platform C++
projects, but then exclusively for a large, Windows only C# project,
I'm getting fed up. I'm fed up with a giant text editor that takes up
100s of MB of ram per instance (and I usually have to have about 5
instances or so open at once). I'm fed up with waiting 5 minutes while
the GUI is locked so it can re-parse all its XML files when they get
updated by SVN. I'm fed up with not being able to search for files by
their name! In the back of my mind, I've been thinking: "How hard
could it be to write a little Python script to add a new source file
to the project XML?"

And this brings me to the Pragmatic Programmer. In the last few weeks
there have been a few postings on Hacker News restating a lot of the
advice from The Pragmatic Programmer, which has me thinking about
"using a single editor well." Over the past year or so I've been
working on more projects at home again, which means programming on a
Mac. I bought a copy of TextMate, and I really like it. But it's not
the same as Notepad++ in Windows, and it's not the same as Emacs in
Linux, and the more I start switching back and forth again, the more I
want to stay in one editor.

So this weekend I installed Emacs via
[homebrew](http://mxcl.github.com/homebrew/). I also got
[package.el](https://github.com/technomancy/package.el), which is
pretty cool, and included in Emacs 24 (I wish I had it a long time
ago; maybe I wouldn't have left the fold). There are a few things I
never really learned, even when I was using Emacs pretty consistently:
I never really got regions down, and never really focused on learning
lisp/elisp well enough to really take advantage of a lot of Emacs's
power. I'll try installing it again at work, and, maybe someday, I'll
have enough time to write that little script and get rid of Visual
Studio for good.

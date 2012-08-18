---
title: Learning to Function
layout: post
tags: coding languages
---
One of my ulterior motives for taking up Emacs again was to have an
excuse to work in lisp (at least a little bit). Everybody seems to be
raving about functional languages again, and they are one of the
things I never really "got" in college (and never pursued afterward).

At work I was trying to switch to use Emacs for working on C#
projects, and I thought it'd be really nice if I could C-x C-e to
compile the project. The only problem is that, to rebuild the whole
project, you need to run MSBuild on the "Solution" file, but that
often won't be in the same directory as the file you're editing.

It seemed like a simple enough problem to search "up" from the current
directory to find the first Solution file, and then pass that in to
MSBuild. One of the rules I set up for myself, though, was to try to
write it as "functionally" as possible (i.e. by not declaring any
variables). My first attempt looked something like this:

	(defun find-solution (dir)
		(if (directory-files dir nil "\.sln$")
			dir
			(if (not (equal dir (expand-file-name ".." dir)))
				(find-solution (expand-file-name ".." dir))
				)
			)
		)

Which returns the directory containing the file, but not the solution
file itself. That if statement is also pretty ugly and isn't very DRY,
since it's repeating `expand-file-name`. 

First thing's first is to get the directory and the file. If I was
writing it in Python, it would look something like:

	match_files = ... # filter with some lambda in it
	if match_files is not None:
		return os.path.join(dir, match_files[0])
	else:
		find_solution(..)
		
Which, despite having the word "lambda" in it, isn't very functional
(it has a variable in it!) To get around that, I cheated and decided
to push that logic to a function:

	(defun file-if-exists (file dir)
		(if file
			(expand-file-name file dir)
			nil
			)
		)
		
	(defun find-solution (dir)
		(or
			(file-if-exists (car (directory-files dir nil "\.sln$")) dir)
			(if (not (equal dir (expand-file-name ".." dir)))
				(find-solution (expand-file-name ".." dir))
				)
			)
		)
		
Looking at the implementation of `file-if-exists`, it seemed like a
pretty useful operation. I Googled around for a standard
implementation, but couldn't find one, so I wrote my own (taking the
function to apply as an argument).

It didn't seem like the best solution. Really, I'm cheating: the
parameters are like variables, and named-functions are definitely
cheating! I pretty much gave up, and moved on to tying it into
csharp-mode, when (while looking for something else) I was reminded of
the `mapcar` function.

Of course! I use `map` all the time in Python and C# (where it goes by
the SQLish `Select`), and it even comes from the functional language
world. Here it is again, with the map: 
		
	(defun find-solution (dir)
		"Search up the tree until you find a .sln file"
		(or 
			(car (mapcar (lambda (file)
				(expand-file-name file dir))
				(directory-files dir nil "\.sln$")))
			(if (not (equal dir (expand-file-name ".." dir)))
				(find-solution (expand-file-name ".." dir)
					)
				)
			)
		)

Now there's only that if statement left, but what I really want to get
rid of is the repeated call to `expand-file-name`. Lets see if
`mapcar` can save us again:

	(defun find-solution (dir)
		"Search up the tree until you find a .sln file"
		(car
			(or 
				(mapcar (lambda (file)
					(expand-file-name file dir))
					(directory-files dir nil "\.sln$"))
				(mapcar (lambda (upper)
					(find-solution upper))
					(delq dir (list (expand-file-name ".." dir)))
				)
			)
		)
	)

It feels a little dirty to be using delq and mapcar on something that
isn't naturally a list (a single string; note that I have to
explicitly create the list), but maybe that's just my unfamiliarity
with the language. They did name it Lisp for a reason.

Since this is my first time out with Lisp, let me know if you can
think of a better way to do this. I'd love to hear about it!

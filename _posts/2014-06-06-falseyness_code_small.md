---
title: Falsey-ness is a Code Smell
layout: post
tags: coding
---

After a recent code review, my co-workers and I had a rather animated
discussion about the merits of relying on falsey-ness in
code. Specifically, we were debating whether it was best-practice to
rely on the fact that most languages treat certain values as `false`
when evaluating the variable in a boolean context. I was against
relying on this, prefering to check for the specific falsey value you
expect, and pretty much everybody else was for it.

Since we've had this discussion before, and it's difficult to
illustrate all my points in person, I wanted to make my case on my
blog.

## A leaky abstraction ##

The prototypical falsey value is the integer 0, and it comes to us
that way from our computers' instruction sets. Computers don't have
boolean types, because they tend to operate on values stored in
registers, which are, mostly, of a fixed size (32 or 64 bits, these
days). I don't know of any computers with a `Jump on false` or `Jump
on true` instruction. Instead, they have things like `Jump on zero`,
or `Jump not zero`.

Lower level langauges, like C, just take this idea and run with
it. They don't really provide many types that the hardware doesn't
natively understand, so they don't provide a boolean type at all. The
reason the C code:

{% highlight c %}
int x = 0;

if( x ) {
    /* ... */
}
{% endhighlight %}

treats the integer `x` as a boolean is because the `if` statement is
compling into a `Jump if zero` instruction (jumping past the body of
the if).

This makes the language simpler, and the compiler easier to implement,
but it forces the programmer to deal with a leaky abstraction. When
you're writing boolean logic, you're thinking of values as only `true`
or `false`, but the language is forcing you to consider the fact that
the computer doesn't actually understand those values at all, even
though that might not be relevant to the task at hand.

## Program in the language of your domain ##

There are two points where my coworkers suggested it was unreasonable
for me to not want to rely on falsey-ness. The first is in arithmetic:
specifically checking for divisibility using the modulo
operator. Surely I must be OK with this construction, they said:

{% highlight c %}
int x = 5;

if( x % 2 ) {
    printf("X is odd!\n");
}
{% endhighlight %}

The problem I have here is that, to paraphrase from the Pragmatic
Programmer, you should be "programming in the language of the domain"
(that is, in the language of the problem you're trying to solve, not
in the language of programming).

Your math teacher never told you "X is divisible by 2 if the remainder
of X / 2 is false," because that sentence doesn't make any sense! In
mathematics, integers aren't true or false, so it would be devolving
into programmer-speak to start considering certain numbers to be true
or false. Explicitly checking for `x % 2 != 0` is keeping the code in
the language of its domain, which is mathematics.

This same concept also applies to the common practice of treating
empty arrays as false. For example, JavaScript developers often write:

{% highlight javascript %}
var things = [];

if (!things.length) {
    console.log("You didn't give me anything!");
}
{% endhighlight %}

Other languages, like Python, treat an empty array itself as being
false:

{% highlight python %}
things = []

if not things:
    print "You didn't give me anything!"
{% endhighlight %}

In both of these cases, the language of your domain is lists of things
(possibly already lower level than you would ideally be writing), and
it does not make sense to consider the length, or the array itself, as
false. I don't tell my wife "The grocery shopping went fast, there
were false people in line at the checkout!" Similarly, if somebody
were to ask you "What Faberge eggs do you have?" you would probably
not answer "false."

For things like strings and lists, I would actually rather have an
`empty` function, which abstracts away the concept of a length
of 0. This is especially useful when length isn't as well defined: is
the length of a Unicode string the number of characters, or bytes? Is
the length of a node in a tree the number of direct children, or all
children?

## Code complexity ##

The next complaint leveled against me was that my code was "more
complex" for being explicit about the condition being checked for. In
particular, we were talking about `if value is not None` versus `if
value`, and the argument was made that the explicit case was "more
complex" because there was "more code." I would argue, in fact, that
there was not more code, and it is actually *less* complex to be
explicit.

For example, consider the following two functions:

{% highlight python %}
def noop():
    pass

def do_nothing():
    pass
{% endhighlight %}

I think it would be difficult to argue that the second version is
"more code" just because the function name is longer. I think it is
also *impossible* to argue that the second function is more *complex*
than the first because the function name is longer.

In the case of the falsey comparison, I think it is actually
considerably more complex than the explicit version, and here's why:

{% highlight python %}
if value is not None:
    print "We have something!"

if value is not None and \
        value != 0 and \
        value != [] and \
        value != {} and \
        value.__nonzero__():
    print "We have something!"
{% endhighlight %}

The second version is, roughly, what `if value` *actually* means in
Python, and, written out that way, certainly looks a lot more
complicated than the first `if` statement.

## Principle of least surprise ##

One of the things that bothers me most about treating non-boolean
values as falsey is that it violates the principle of least
surprise.

When reading a piece of code, say a function, you have to operate on
the assumption that the values have a fixed set of properties and
behaviors, and that those properties and behaviors constitute a
meaningful *type* of value. In a dynamically typed language, you (and
the interpreter) use duck typing to define this type. That is, the
type must contain all the properties accessed on the type by the
code. In a statically typed language, the language requires the code
to declare the type of the value and enumerate the exact properties
and behaviors of that type.

In order to make the code more legible, it helps if, at least for the
block being read, the type of the value represents some meaningful
concept in the program. For example, an integer is a meaningful
concept, understood by most people reading your code, and on which the
standard arithmetic operators are well defined. If you are writing
software for a human resources department, an `Employee` is meaningful
type (whether you define it or not), and there are reasonable
assumptions that can be made about the properties and behaviors of
that type.

As I discussed earlier, having a boolean state usually doesn't
actually make sense for whatever conceptual type a value is supposed
to have. When reading the HR code, I would expect an `Employee` to
have a name, an ID, a mail stop, etc. I would not expect the employee
to be `false` or `true`. When it comes to things like numbers and
empty lists and nullable values, we've tricked ourselves into thinking
that there are reasonable boolean representations of these types
because computers, and programming languages, have provided them for
so long. But, if we think about these types in terms of their
real-world counterparts, we see that it really *doesn't* make sense
for many of them to exhibit boolean behavior.

## When it makes sense ##

Of course, I do believe there are a few instances where a type can
have a reasonable boolean representation. As an example, consider
Python's regular expression library: when you call `re.match()` you
get a `MatchObject` back, or `None` if the string did not match the
RE.

Consider, instead, a version which *always* returned a `MatchObject`,
but which evaluated as `False` if there was no match. In that case,
you could still write code like this:

{% highlight python %}
if re.match(r"\w+", value):
    print "Value contains at least one word"
{% endhighlight %}

But, you could also write code like this:

{% highlight python %}
def print_sub_groups(regexp, value):
    match = re.match(regexp, value)
    group = 1

    try:
        while True:
            print match.group(group)
            group += 1
    except IndexError:
        pass
{% endhighlight %}

In the specific domain of an object representing the result of
matching a string against a regular expression, it actually does make
sense for the match to be `false`, if there is no match. It also has
the added benefit of allowing the programmer to not have to
special-case whether the match was successful or not if it isn't
relevant to their code.

## Avoiding sentinel values ##

Which leads to the title of this post, which is that treating values
as falsey is often a code smell that you might be doing something else
wrong.

Let's consider a (very abridged) version of the code that initially
started the discussion with my coworkers. The idea was that a function
was returning a list of URLs which would ultimately be sent back to a
client browser. In certain situations, in order to work around bugs in
older OSes, we wanted to downgrade the connection from HTTPS to HTTP
(when the initial request was already over HTTP). The code looked
something like this:

{% highlight python %}
def gather_urls(resource_ids, protocol_override=None):
    resources = db.get_resources(resource_ids)
    resource_urls = []

    for resource in resources:
        url = resource.get_url()

        if protocol_override:
            url = replace_protocol(protocol_override, url)

        resource_urls.append(url)

    return resource_urls
{% endhighlight %}

The line in contention was the `if protocol_override`, of course. The
code calling this function determined if the protocol needed to be
overridden, otherwise this function should return the pre-configured
URLs for the resources.

The smell here is that this code has to be told whether or not to
override the URLs at all, something it isn't particularly concerned
with (it was actually gathering more than just URLs from the
resources). Instead of even needing to check whether the protocol
needs to be overridden (the responsibility of other code), the code
could be rewritten like this:

{% highlight python %}
def identity_filter(url):
    return url

def force_protocol(protocol, url):
    return replace_protocol(protocol, url)

force_http = partial(force_protocol, 'http')


def gather_urls(resource_ids, url_filter=identity_filter):
    # ...
    url = url_filter(url)
    # ...
{% endhighlight %}

This code has a few advantages over the previous implementation:
first, the decision of whether or not to override the protocol is
completely up to the calling code, with no conditionals. Second, how
you actually change the URL has been moved out of the function, so
other URL filters can be constructed, or you could even chain filters
together.

Hidden behind the falsey check in the first version was actually a
sentinel value, `None`, which pointed to a larger problem with the
implementation. Most falsey checks are really checks for a particular
falsey value (or maybe a couple falsey values), which all send the
code down a different code path.

## Style ##

In the end, most of this comes down to personal preference. Except for
a few corner cases, where the programmer doesn't anticipate a
particular falsey value being passed in, both styles of code are
correct.

To me, the implicit check for falsey values reads like a number
without a unit. The programmer is probably checking for a particular
value, but just left it off as short-hand. I find it aggravatingly
terse, and I have to stop and think what the programmer meant. If I'm
debugging a problem with a particular input, I have to consider what
the current value is, whether it would evaluate as false, and whether
that is the correct behavior.

This is more difficult if you have to switch back-and-forth between
languages often, because not all languages interpret the same values
as falsey. For example, in Perl, `"0"` is falsey, but `"0.0"` is not
(of course!). There is something to be said for taking full advantage
of the language you're using (instead of limiting yourself to the
least common denominator of all languages), but relying on certain
esoteric, and implicit, rules might make the code more difficult to
maintain without any clear advantage.

Expressiveness is often used as an argument for allowing falseyness,
and it's certainly more expressive of a language to allow values to be
falsey. However, there is a difference between expressiveness and
clarity: just because a language allows you to express the phrase "if
this list is false" does not mean that that is the clearest means of
expressing what you actually mean ("if this list is empty," or,
better, "if there are no apples").

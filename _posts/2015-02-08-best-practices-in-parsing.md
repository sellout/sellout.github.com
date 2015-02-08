---
layout: post
title: "Best Practices in Parsing?"
description: ""
category: 
tags: []
published: false
---
{% include JB/setup %}

* some form of invertible syntax
  * I parametrize on token type
  * whitespace combinators need to be parameterized on set of WS tokens
  * inserting line breaks and alignment?
    * break at “shallowest” whitespace marker?
* popular algorithms?
  * I use derivatives for user-extensible parsers, because it allows for ambiguity (necessary) and supports any CFG (desirable)


Parsing is definitely not a solved problem, but there have got to be some best practices out there, right? I’m going to lay out what I reach for when working on a parser as a first pass as maybe what we should be doing, but mostly to solicit feedback so I can figure out what I need to change – and hopefully to push some improvements through to the existing libraries out there.

As a general approach, I reach for combinators rather than YACC-style generators. Without this choice, what follows would be pretty much impossible. I have also stopped using any sort of lexer – I do a single parsing pass and that’s it. No worries about not being able to use the same sequence of characters in two disjoint contexts.

Now, given the decision to use combinators, I build on [invertible syntax descriptions](http://lambda-the-ultimate.org/node/4191). I’ve implemented this library in PureScript, Scala, Common Lisp, and Haskell (yes, Haskell, because I do some parts a bit differently than they do) and love the approach. Why should I write basically the same thing twice, but differently enough that I am bound to have some inconsistencies between the parser and pretty-printer. However, there are also some things that I haven’t solved – mostly involving whitespace, like where to break lines and how to handle indentation.

As far as where I differ from the paper, I took a note from [David Darais’ implementation of derivative-based parsing](http://hackage.haskell.org/package/derp) and parameterized `Syntax` on the token type. This allows you to use chars directly, or to use a lexer with your syntax description. I have also extended Syntax in various ways to add combinators and to make it possible to override default implementations (that’s something else I waffle on, but that’s a story for another post).

Now we can write a wonderful syntax that looks like `just <$> someExpr <|> otherExpr`, but what do we do with it?

First, we sigh. We sigh because no parser library or pretty printer library (do those even exist?) is ever based on invertible syntax. In Haskell, that’s not a big deal, just implement some type classes, but it still bugs me. Whatever.

To some extent, you need to think about your parser algorithm when you’re writing your syntax description, just as you did when you didn’t get a pretty-printer for free. So, I tend to think of invertible syntax like that – same old parsers, but I get pretty-printing for free.

Here is the part I struggle with the most – choosing the parsing algorithm. As I often do with these kinds of decisions, I punt. I want an algorithm that will handle _any_ CFG. I don’t want to worry about left recursion, the size of my lookahead, etc. I just want to write the grammar that seems “right” to me. So, I dive right into [derivative parsing](http://lambda-the-ultimate.org/node/4159) until it bites me and I’m sitting around waiting for minutes for something to parse. But, there are other pluses that draw me toward derivatives – it makes it easy to design an _extensible_ parser. Since there are no issues with left recursion or anything, you don’t have to be able to analyze the whole grammar to ensure that there are no cycles. And you can allow users to extend the grammar without expecting them to be experts in parsers.

Now, most people using parser generators aren’t writing extensible parsers … so, what do you go to then? Parsec? It seems to be the de facto standard. I actually don’t pay too much attention to the trade-offs between different approaches. I did at one point, but now I’m like “Earley? Packrat? Yeah … one of those is probably good.”

My real question is whether invertible syntax is the magical thing it seems to be for me, and if so, why 1) doesn’t it exist more places and 2) don’t more combinator libraries build on it? Even without invertible syntax, just a Parser type class would be helpful in terms of experimenting with different algorithms. I guess part of the answer to #1 is that I should be publishing my versions of this invertible syntax stuff … it’s just that the gap between “useful to me” and “useful to others” is bigger than I’ve wanted to deal with, and maybe this post is a first step toward getting over that hurdle.

Questions: (these should be bold in the preceding post)
* Combinators vs YACC
* skip the lexer?
* invertible syntax
* best algos/libraries for various purposes

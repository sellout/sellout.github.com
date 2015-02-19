---
layout: post
title: "Teaching from Posets"
description: ""
category: 
tags: []
published: false
---
{% include JB/setup %}

When I am learning a topic, I find I often (sometimes quite explicitly) build a poset to help me categorize various approaches to the problem. For example, in [my recent post about parsing]({% post_url 2015-02-08-best-practices-in-parsing %}) I mention a few different algorithms, and that I prefer derivates. In practice, this means that I usually start by using a derivative parser, but then reconsider that decision if I hit various issues. This is one of the reasons having a good abstraction (like [syntax descriptions](http://hackage.haskell.org/package/invertible-syntax)) is necessary – switching between two algorithms that share a common interface is much easier than switching between different interfaces.

So, back to the parsing example. I start with derivatives for one reason in particular – when I am beginning, I want an unconstrained space.

I think of most things as some kind of poset, that is, some things are just better/worse than some other things and those same things may involve tradeoffs with different other things, such that neither is strictly better than the other. A bit more specifically, I tend to think of things as meet semilattices – often, the meet won’t actually exist, but it’s usually easy to see it as the intersection of two elements.

In some cases, this lattice is very subjective – EG, if you try to make a lattice of programming languages, you’re going to have many different lattices – type systems, concurrency models, etc. – that don’t all line up in the first place, then you are necessarily focusing on some subset of those when you make a poset for full-fledged languages. And even those individual posets are complicated, because languages usually differ at least a little from formal definitions of their various aspects, and there is very little proof/verification done on what is implemented, vs the nice formal models.

Anyway … usually when we learn things, we are taught in some sort of chronological order. It’s somewhat post-hoc, in that it’s not necessarily the order things were created in, but the advancement of concepts usually has a half-decent correlation with time.

Also, this makes sense in the context of a focused course, where you are aiming for depth in a narrow area. But in terms of stuff up on the Intarwebs, we mostly want to ensure that someone reading the first paragraph on Wikipedia and then copying/implementing it gets something reasonable done.

This is going to involve value judgements.

This is where the poset stuff comes in. Instead of writing a history of, say, models of computation, take the lattice of computation, eliminate everything except the maximal elements, and focus on those. Be clear about the tradeoffs between them, but try to pick a default. For me, with parsing, that default is derivatives. Because its tradeoff is performance, and I rank performance pretty low. Basically, by the time I know whether I need better performance, I’ve figured out which other features I can live without, and that helps me navigate the tradeoffs.

Depending on how “real” these lattices are (a lattice of parsing algorithms is going to be more legit than a lattice of programming languages), the things lower on the lattice are more or less historical context. And using a _reverse_ chronological order can be better than forward. Rather than constructing a false bright line forward through time, ignoring myriad dead ends and the true fits and starts of invention, you can frame each previous step in terms of how it motivated the current state of things.

I mentioned models of computation above. The one most people are familiar with is λCalculus (or the universal Turing machine – the two are equivalent). There are many others, and while “Turing complete” has been splashed around to make it sound like there’s no difference, a lattice of expressivity has been built up (largely due to the efforts of ????) that shows real differences between the models. At the top of this semilattice are a number of models, my default is the kell calculus, mostly because it has the smallest grammar and I don’t really understand the various tradeoffs with the others (which is complicated by the very confusing relationships between various calculi in the ambient family). This paragraph is meant to be a plug for Kilns … it is currently failing.

To summarize:
1. open with the maximal elements,
2. make a judgement call – pick one of the maximals as your basis for comparison, and
3. present non-maximal elements via their influence on elements higher in the lattice.

* “write your conclusion first”
* resumes are reverse chrono, most relevant first & easy to update

* [poset]: partially-ordered set
* [semilattice]: a poset where any two elements (which may not be directly comparable) share some common value that is ≤ both of them

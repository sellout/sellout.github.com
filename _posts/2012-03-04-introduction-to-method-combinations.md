---
layout: post
published: false
title: An Introduction to Method Combinations
tags: method-combinations
---

For some reason, method combinations have never been a very popular feature of Common Lisp.

A very cool feature of Common Lisp is the “method combination”. Even in their most basic form, they provide functionality beyond that of most OO systems. However, beyond the `STANDARD` method combination, they are somewhat arcane – which is probably because of a paucity of examples of their use. [The ANSI CL spec]() gives [a couple examples](http://www.lispworks.com/documentation/lw50/CLHS/Body/m_defi_4.htm). They include `EXAMPLE-METHOD-COMBINATION` (which doesn't have the best name, but uses integers as the qualifiers and sorts the methods by that qualifier) and `PROGN-WITH-LOCK` (which “locks” the first argument during execution – somewhat like the `synchronized` keyword in Java – would probably be more useful as `STANDARD-WITH-LOCK`, but `PROGN` keeps the example small). And [Sonya Keene's book](http://books.google.com/books/about/Object_oriented_programming_in_Common_LI.html?id=waVQAAAAMAAJ) gives only the most rudimentary sketch of a possible use. She even goes so far as to say: “Because we believe that most applications will fit well with one of the built-in method combination types, we do not cover the syntax of the long form of define-method-comination in this book.” The first real example that I'm aware of in the literature is from [Paul Graham](http://www.paulgraham.com/)’s [On Lisp](http://unintelligible.org/onlisp/onlisp.html) – and [his example](http://unintelligible.org/onlisp/onlisp.html#SEC161) (starting with “This way of combining methods”) is regularly [derided](https://twitter.com/smuglisp_borat/status/166578604304629760). [Peter Seibel](http://www.gigamonkeys.com/) finally gives [a real example](http://www.gigamonkeys.com/book/practical-parsing-binary-files.html#adding-inheritance-and-tagged-structures) in [Practical Common Lisp](http://www.gigamonkeys.com/book/). And that’s all I'm aware of. There could be others – there are many Common Lisp books that I haven’t read, but I think I covered the well-known ones.

Alan Crowe wrote [an article on the use of `DEFINE-METHOD-COMBINATION`](http://www.cawtech.demon.co.uk/clos/define-method-combination.html) where he starts with the “simplest possible method combination”, `MOST-SPECIFIC`, which executes only the most-specific method, then moves to `RUN-ALL`, which seem to be like the built-in `LIST` combination (except missing `:AROUND` methods). He gets a little wacky with `REPORT-FIRST-ARGUMENT` which doesn't call any of the methods, but rather just prints the first argument. With `POLYNOMIAL`, it starts to look interesting, but it’s still very special-purpose. I can’t imagine anything beyond that single use case. But why shouldn’t method combinations be a tool that Lisp programmers are familiar enough with that we can make one-off use cases and not be afraid that our code will scare others?

I think this is a shame, because beyond the `STANDARD` method combination, there is a lot of possibility. Part of the problem is that the built-in method combinations are mostly uninteresting. Of the nine additional built-in combinations, I could only find examples of five (and at least one of those is questionable). Also, the built-in combinations also remove the ability to use `:before` and `:after` methods (but do preserve `:around`). However, it is also possible to define additional non-built-in method combinations that can do almost anything, and that is where it gets interesting.


First, we will discuss what's already out there, and how it's used. It turns out that I found about as many uses of built-in combinations being used as I found custom combinations. Even with my claim that the built-ins are not the most useful method combinations, this seems surprisingly low, since their use requires no libraries and they are pretty easy to comprehend.

A brief explanation: when you define a generic function, the `:METHOD-COMBINATION` option lets you specify which combination you want to use.

```common-lisp
(defgeneric foo (x)
  (:method-combination progn))
```

will give you a function that uses the progn combination. This means you can define two kinds of methods on it, `PROGN` and `:AROUND`.

```common-lisp
(defmethod foo :around (x)
  (print "object info:")
  (call-next-method))
(defmethod foo progn ((x sequence)) (print (length x)))
(defmethod foo progn ((x vector))   (print (array-element-type x))
```

All of the non-`STANDARD` built-in combinations have the same pattern. 
Here are the apparently useful built-in combinations, in order of popularity.

`PROGN`
:   As seen in the example above, this is usually used for side-effects. If you are serializing objects to a stream, or for set-up/tear-down functions, such as in the [LIFT]() testing framework, or when dealing with sockets, locks, or other resources that may need to be acquired/released in sequence.

`APPEND` and `NCONC`
:   Yes, these are two different combinations that I am lumping together because they largely accomplish the same thing, one just with a bit of danger. For the record, `APPEND` is the far more common of the two; I could only find one example of `NCONC`. This is my favorite built-in combination. This is like the functional version of the `PROGN` combination. It's often used to collect slots or other things where pieces of the answer are held in different slots up the hierarchy. 

`AND`
:   This is useful in cases where each class may add extra conditions on some function. One use case could be for checking the state of some object, imagine a LIVEP function where a CHANNEL class might only return whether it's enabled/disabled, but the NETWORK-CHANNEL subclass adds the condition that the socket is connected.

`+`
:   Thankfully, someone has stepped up to the plate and come up with a better example than Paul Graham's JACKET and TROUSERS. [fink]() is a Go AI that uses the `+` combination to calculate the score for a particular configuration.

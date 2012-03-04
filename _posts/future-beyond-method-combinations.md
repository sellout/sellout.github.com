---
layout: post
publish: false
title: Beyond Method Combinations
tags: method-combinations
---

Up to this point I’ve been lying to you – remember [ContextL](http://common-lisp.net/project/closer/contextl.html)? It doesn't use method combinations. [Filtered Functions](http://common-lisp.net/project/closer/filtered.html)? It doesn't use method combinations (at least not in an interesting way).

If these things don't use method combinations, how do they manage to do things that _look_ like method combinations?

The answer to this question takes us beyond the bounds of [ANSI Common Lisp](http://www.lispworks.com/documentation/HyperSpec/Front/) and into the realm of the [Meta-Object Protocol](http://www.alu.org/mop/index.html) (MOP). Most implementations of Common Lisp support the MOP, but not always completely or consistently. The [Closer to MOP](http://common-lisp.net/project/closer/closer-mop.html) library aims to make MOP support portable (supported implementations are listed on its site). It’s recommended to use this library if you plan on making any use of the MOP.

With the addition of the MOP, `DEFINE-METHOD-COMBINATION` may appear to be obsolete – after all, the MOP can do everything `D-M-C` can.  ([`COMPUTE-EFFECTIVE-METHOD`](http://www.alu.org/mop/dictionary.html#compute-effective-method) is where the method combination magic happens.) Both ContextL and Filtered Functions use the MOP in lieu of method combinations. However, method combinations are a simpler mechanism where they fit.

[Quid Pro Quo](https://github.com/sellout/quid-pro-quo) uses the MOP _and_ method combinations together – a method combination does the bulk of the work, but a custom metaclass handles defining invariant methods on all slot accessors.

There are also uses of the MOP that don't look anything like method combinations. [Clozure CL](http://ccl.clozure.com/) uses the MOP to [make CLOS fit Objective-C's OO model](http://ccl.clozure.com/manual/chapter14.5.html) for a very lispy bridge.

We won’t get into most of the features of the MOP, but what I’d like to discuss is places where you might find yourself hitting the boundaries of method combinations.

As I mentioned, Filtered Functions uses method combinations, but only in a minor way. Really, it shouldn't need to use them at all, but it’s a bit of a trick to avoid having to define `COMPUTE-EFFECTIVE-METHOD`. Functions using the `FILTERED` combination allow the `STANDARD` method qualifiers as well as the filtering qualifiers:

```common-lisp
(defmethod eval :around :filter :first ((form (eql 'lambda)))
  ...)
```

FF defines `COMPUTE-APPLICABLE-METHODS` on its functions, which checks to see if the filter matches each of the methods. After filtering the methods, it can pass them along, and `STANDARD` method combination applies.

Except it doesn't. The `STANDARD` combination will only match a method with _exactly_ the right qualifiers, and we can't delete the filtering qualifiers, because the `METHOD-QUALIFIERS` function is defined to return all of them (and that same function may be used by the `STANDARD` combination to group the methods). So the `FILTERED` combination just relaxes the method-group-specifiers, ignoring the filtered qualifiers and then handling the methods as in the `STANDARD` combination.

Since this seems like a very useful pattern for qualifiers that can't easily be handled with method combinations, the [Sudafed](https://github.com/sellout/method-combination-utilities) library defines the `LAX` combination for this purpose.

```common-lisp
(define-method-combination lax ()
  ((around (:around . *))
   (before (:before . *))
   (after (:after . *) :order :most-specific-last)
   (primary * :required t))
  (combine-standard-methods primary around before after))
```

Now, what does Filtered Functions do that can't be done with method combinations? FF replaces the specializer with the discriminating value. While the qualifiers and specializers are all available during method combination, `COMPUTE-APPLICABLE-METHODS` runs before method combination, and without special handling, it will consider the methods non-applicable because the specializers aren't correct. So the method combination never sees those methods.

[Reasonable-Utilities](https://github.com/vseloved/rutils/blob/24583101a54cb8c65391d852cc3d31a6005c81a0/experimental.lisp)’ `MULTI` combination provides something similar to Filtered Functions, but the discriminating value is stored as a qualifier, so it can be implemented as a combination. Here is a modified version that behaves a bit more like Filtered Functions than the original:


```common-lisp
(define-method-combination multi (dispatch-fn)
  ((all *))
  (:arguments &whole args)
  (let ((matching-methods (remove-if-not (lambda (qualifiers)
                                           (find (apply dispatch-fn args)
                                                 qualifiers))
                                         all
                                         :key #'method-qualifiers)))
    `(call-method ,(first matching-methods) ,(rest matching-methods))))
```

This is meant to replicate [Clojure](http://clojure.org/)’s [`defmulti`](http://clojure.org/multimethods), but it wouldn't be difficult to extend it with `:AROUND`/`:BEFORE`/`:AFTER` methods.

So you can see that based on various design decisions (in this case, whether the discriminating value is encoded as a specializer or not), you may or may not be able to use method combinations. And as your library grows and improves, you might come to the point where method combinations are no longer an option, and you have to take advantage of the MOP.

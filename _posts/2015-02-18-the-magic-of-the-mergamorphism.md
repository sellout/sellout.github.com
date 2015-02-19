---
layout: post
title: "The Magic of the Mergamorphism"
description: ""
category: 
tags: []
published: false
---
{% include JB/setup %}

* Stuff about recursion schemes.

# Folds, unfolds, refolds, reunfolds, and some uncategorized

There are many different recursion schemes, but they can mostly be grouped into a few categories based on the general shape.

There is one shape that I feel like I run into a lot, but hasn’t seem to have appeared in the literature

# Combinations

These are recursive operations on two functors of the same type. The general shape is:

```haskell
algebra f a -> Fix f -> Fix f -> a
```

Note how similar this is to the fold shape. The only difference is the second `Fix f` parameter.

A perhaps uninteresting case is the zippamorphism. Its algebra is simply `f a -> a`, which you may recognize from the typical catamorphism. It requires that the functor implements `Zip`, and then performs the algebra on the successfully zipped functor.

Now, the meat of the matter: the mergamorphism. Merging is basically zipping that can fail. This is based on the `Merge` type class that I created, which is basically `Zip`, but with the result in `Maybe` There is a default instance of `Merge` for any functor that implements `Traverse`. The algebra here is a bit more complicated, `(f (Fix f) ✕ f (Fix f)) ∨ f a -> a` The right side of the disjunction looks just like the zippamorphism, the left side is the result of a failure to merge the two functors.

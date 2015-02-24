---
layout: post
title: "FUNCTOR _ALL_ THE TYPES!"
description: ""
category: 
tags: []
published: false
---
{% include JB/setup %}

In my mind, my current job was offered to me at the moment I showed that I had a clue about fixed-point data types (which is good, because I certainly didn’t have a clue about anything else the company was doing). So, since this is clearly what got me employed, obviously it is the most important thing for you to learn to further your own career. With that unassailable lead-in, let’s learn how to …

# FUNCTOR _ALL_ THE TYPES!

Basically, turning many of your data types into functors can simplify your code while making it more flexible and more efficient. This post provides background and motivation, while the next one will go into more depth for how you can use this approach effectively.

## the Y combinator

The Y combinator is something that seems to get a lot more talk than use. If you’re not familiar with it, it is a way to implement recursion without having access to it. For example, given the usual factorial function:

```haskell
fact n = if (n <= 1) n (n * fact (n - 1))
```

What if for some reason, you couldn’t refer to the name of the function in the scope of its definition – this really isn’t so uncommon. You’d maybe come up with something like:

```haskell
fact' r n = if (n <= 1) n (n * r (n - 1))
```

Here the recursive call has been replaced by another parameter, `r`, that is given the function to use for the next call. Now, how to call that?

```haskell
fact = fact' fact'
```

Hrmm, but that second `fact'` is expecting two parameters … so maybe you try `fact = fact' (fact' fact')` – but now the last one has the same problem. This is getting nowhere fast. Here is where the Y combinator shows its stuff:

```haskell
Y f = (\x -> f (x x)) (\x -> f (x x))
```

the meaning of this might not be obvious, but it is an expression equivalent to `Y f = f (Y f)` that avoids using recursion (because, remember, recursion isn’t available to us).

Now, with this definition, we can get past our little hurdle:

```haskell
fact = Y fact'
```

If we take the recursive version of `Y` and expand it a few times, it looks like:

```haskell
f (f (f (f (Y f))))
```

which if we replace `f` with `fact'` looks a lot like the infinite definition we were trying to get to before (`Y f` is basically the λCalculus version of “…”).

So, this has given us a simple way of adding recursion to a system without – you first write a non-recursive version, replacing the recursive calls with a function parameter, and you then use the Y combinator to provide the function itself as the argument for that parameter.

The Y combinator is also known as a fixed-point combinator. But there are many fixed-point combinators. If you got this far and are wondering how that `Y f` call doesn‘t spin off into infinity, you might be interested in the Z combinator, which is the same as the Y combinator, but works in languages with eager evaluation.

# from functions to data types

```haskell
data Json = JNull
          | JBool Bool
          | JStr String
          | JNum Real
          | JArr (List Json)
          | JObj (Map String Json)
```

Ok, here’s a nice implementation of JSON (I know, I forgot the parser and serializer – eh, minor detail). If you aren’t familiar with Haskell syntax, you can roughly read this as a `Json` interface, with subclasses named `JBool`, `JStr`, etc. and those subclasses have fields that match the types after them (EG, `JArr` has a field that’s a list of other `Json` objects).

You can see that the data structure is mostly flat, but `JArr` and `JObj` refer to `Json` recursively. Of course, that is perfectly fine in Haskell (or anywhere else we might want to define these structures), but might there be some benefit to a fixed-point approach? Let’s see.

```haskell
data JsonF a = JNull
             | JBool Bool
             | JStr String
             | JNum Real
             | JArr (List a)
             | JObj (Map String a)
```

The first step was simple – we’ve replaced the recursive reference with a new type parameters (aka, generics/templates/whatever). Like with the function case, trying to pass the type itself as the parameter gets us into infinite regression, so we need something like the Y combinator. The basic form for this is `Fix` (there are some useful more restricted versions called `Mu` and `Nu`, but we’re not going to worry about those right now).

This shape of a data structure is known as a “functor”. There’s not much to a functor except for the function `map :: Functor f => (a -> b) -> f a -> f b` (which, for those without Haskell experience is a function that takes a function from `a` to `b` and then a functor “containing” `a`s and returns a functor “containing” `b`s – `List` is a common functor, so if you replace `f` with `List`, you can see what you might normally think of as the `map` function).

Unlike with the function case, where we picked up on the Y combinator because recursion wasn’t available to us, here we can use the recursive form of `Fix` directly: `newtype Fix f = Fix (f (Fix f))`. One thing to note here is that the first and third `Fix` refer to the type name (the first defines it, the third is the recursive reference), while the second `Fix` is the name of the constructor to create a value of type `Fix f`.

```haskell
type Json = Fix JsonF
```

## Why bother?

### alternative shapes

The simple case is the `Fix` type that I showed above. That gives us back our primitive recursion. However, there are other fixed-point types available:

* the `Free` monad – this turns each recursive point into _either_ an actual recursive value or some other arbitrary type. `Free JsonF Error` turns our JSON functor into a type that supports partial failure, so now we can implement `parse :: String -> Free JsonF Error`.
* the `Cofree` comonad – this turns each recursive point into _both_ a recursive value and some other arbitrary type, allowing you to add annotations on each node. `Cofree JsonF Int` could let you, I dunno, track how many bytes each node takes in your binary encoding.

I’ve come up with a couple of my own:

* `FaultTolerant`, which is a combination of `Fix` and `Free` – it has a third case (relative to `Free`) that indicates that a branch is successful to the end, eliminating the need to traverse to the leaves looking for failures.
* `Diff`, which is complicated in structure, but unifies two recursive structures, duplicating only the parts of them that differ.

This kind of flexibility is already impressive. Your single data structure is now easily malleable into many related structures, without having to duplicate code and write tedious conversion operations that frustrate you every time you make any adjustment to your data structure.

But that’s not all!

### compositionality

It’s not uncommon to have to move data through a number of formats. EG, you might have an application with an internal `Data` structure, that also handles `Json` and `Xml`, and sometimes you need to convert right through the whole chain. So, you might have functions like `j2d :: Json -> Data` and `d2x :: Data -> Xml`, and you think “well, I can compose these functions very easily, see? `d2x ∘ j2d :: Json -> Xml`!”

Alright, yes, you have a point. But that is costly. First you have to recursively build up the `Data` from the `Json` _then_ build up the `Xml` from the `Data` before you can get rid of that `Data`. However, with the functors, you can avoid that memory use and some extra building/traversing of structures.

```haskell
j2d :: JsonF a -> DataF a
d2x :: DataF a -> XmlF a

d2x ∘ j2d :: JsonF a -> XmlF a
```

Oh man, these structures are so nice, they have a special name – natural transformations!

Now, what this version has done is changed the conversion from one that converts an entire `Json` tree through an entire `Data` tree to an entire `Xml` tree into a conversion from a single `JsonF` node through a single `DataF` node to a single `XmlF` node.

This means that you never build up a `Data` tree. But how do we turn this into the structure we really want? `Json -> Xml`? That brings us to … recursion schemes. For something just like what we have, it’s pretty simple and we can use all kinds of variations, but most directly, just `cata (d2x ∘ j2d)`. For those who _do_ know Haskell, that is just a generalized `foldr`. Of course, you often have other conversions, too. EG, say you store JSON on disk and send XML over the network, so we really want to convert a `String` (of JSON) to a `ByteArray` (of XML).

```haskell
readJson :: String -> JsonF String
sendXml :: XmlF ByteArray -> ByteArray
```

Those signatures look a bit odd, don’t they? I mean, I’m trying to convert _to_ a ByteArray, how did I end up with a `ByteArray` in my XML?

That has to do with the bottom-up nature of these folds. Think about it this way: You start out with `Fix XmlF` (which you can unwind a little to `XmlF (Fix XmlF)`), you then convert all the leaves to ByteArrays. Now, you pop back to the next level up, and you have `XmlF ByteArray` at that level (since you’ve converted all its children. You can now use the `sendXml` function to convert that level, then pop up, use `sendXml` again, etc. And to wrap it up, since the leaves by definition don’t have any children, there’s no difference there between `XmlF (Fix XmlF)` and `XmlF ByteString`, so `sendXml` operates just fine on those leaves we converted first.

But now we want one _efficient_ operation that converts from that `String` on disk to a `ByteArray` over the network.

```haskell
hylo sendXml (d2x ∘ j2d) readJson
```

`hylo` is related to (but a bit more complicated than) the `cata` we saw before. It has three components (from left to right) the folding function, a natural transformation, and the unfolding function. They’re used in right-to-left order, similar to usual function compositon. You can parse that as “we read the JSON, convert it to `Data`, convert that to XML, then send the XML.”

---
layout: post
title: "Response to the RIO monad"
publish: false
description: ""
category: 
tags: []
---
{% include JB/setup %}

This is a response to [The RIO Monad.](https://www.fpcomplete.com/blog/2017/07/the-rio-monad). It currently makes the most sense if read side-by-side with that post.

**NB**: This is a draft that I didn't intend to make public. But it is, so I'll leave it up (with some edits). Apologies for just barely touching on a number of the points I make. Please feel free to ask about them in the comments, and I'll try to expand upon them either here or in separate posts that I'll link from here.

# tl;dr

Avoid monad (transformer) stacks, see them as a hint to refactor, decomposing the operations. Tools like `MonadUnliftIO` can be useful when dealing with code you don't have control over and for keeping technical debt in check, but they should be seen as a stop-gap on the way toward more well-structured code, not a goal on their own.

# pointwise

## What's happening in Stack?

This seems like a great description of how Stack bootstraps itself -- each layer of config informing the next. But I feel like the description belies the need for any kind of hierarchy. E.g., once we load up the logging config, we should turn it into a function that does all the right coloring and forget about the config that mentions coloring and log level. It's in the function now. We should also no longer care about "do we use the system-wide GHC" (`Bool` or otherwise). After we read that, we just have a path (or richer structure) to the GHC we're using.

"Some parts of our code base are pure. We'll ignore those for now, and focus only on the parts that perform some kind of `IO`." -- this is an ominous statement. Even in a rich GUI application, very little of the code should have anything to do with IO. Also, the introduction of `IO` doesn't mean you should change anything else about your code ... but you _should_ still minimize where `IO` shows up. It's a big hammer.

## Pass individual values

-   can be tedious, but, if you need to do a lot of threading to add a new value, that sounds like you should be refactoring anyway
-   passing "random `Bool`s" *is* a problem, but it's unrelated to this approach. [Eliminating boolean blindness](#boolean) is orthogonal to the RIO monad.

## Pass composite values

-   the declaration `someFunc :: Config -> IO ()` looks like trouble already. I would expect functions like
	```haskell
    readConfig :: IO Config
    someFunc :: Config -> SomeTypeToOutput
    
    main :: IO ()
    main = do
      config <- readConfig
      let res = someFunc config
      print $ show res
    ```

## `ReaderT IO`

Monad transformers aren't a solution -- this is a whole thing. Instead of dealing with stacks, they're another place we should be using the pain of awkward types to point us toward a refactoring. Notice the `readConfig` example above. There is `IO` in some places and `Reader` (aka `(->)`) in others, but neither exists in both. This gives you a much more testable API, among other things.

What transformers *are* is a `newtype` for selecting a particular `Monad` instance.

The example in this section finally introduces why `someFunc` has `IO` -- logging. [We'll come back to that](#logging).

## `MonadLogger`

This section is absolutely right.

## Custom `ReaderT`

I don't agree with this section.

The `LoggingT` situation leading to here *is* exactly the kind of thing that should make you [rethink what you're doing](#logging), but newtyping a `newtype` (which is fine in some cases) is not the way to do it.

## Be more general!

`Monad*` classes are a patch over transformers. For most of them, there is *one* "best" representation, akin to the notion of a [universal construction](https://ncatlab.org/nlab/show/universal+construction). E.g., `MonadIO` says, "there is `IO` in this stack." If you take the advice from earlier and use stacks as a hint to refactor, "there is `IO` in this stack" and `IO` are isomorphic.

## `Has` type classes

I've not been a fan of classy optics. I think there are more general multi-parameter type classes (`:<:` and `:>:`) that solve the same problem without having to TH a bunch of new classes for every type.

`HasFoo a ~ Foo :<: a ~ Lens' a Foo`

and

`AsFoo a ~ a :>: Foo ~ Prism' a Foo`

look at that nice duality, and lack of type class proliferation. Each one requires a lawful optic.

I don't think this part is necessarily bad (other than wanting to avoid the type class proliferation), but I don't think the composite values have been justified quite yet.

## Exception handling

Agree with this -- please don't `MonadBaseControl`.

But more generally -- why are you even using execptions? `Either` is the correct answer here. And this could get into a whole thing about duoids (like the relationship `Either` and `Validation` have to each other.

## Introducing `MonadUnliftIO`

unliftio is a good library. I certainly use it (some parts more than others). `MonadUnliftIO` is an improvement over `MonadBaseControl`, and if I ever see the former I immediately replace it.

However, you should avoid `MonadCatch m => StateT s m a` problems by avoiding state stacks, not by adding more magic in the stack classes.

Sometimes, that's out of your control. I have found myself in situations too often where a call to a 3rd-party library gives me back an unholy stack. My options are to replace the library, or do the best I can to insulate the rest of my code from the stack. So I generally do the latter and unliftio can be very helpful there.

But in your own code, you shouldn't build a stack like that.

## More concrete

"We're not writing a generally-consumable library. We're writing an application." -- This is the wrong mindset. It forces contributors (both different teams on a corporate project and someone trying to make a PR in OSS) to have much more of the project in their head in order to make a change. An application should be thought of as a number of interacting libraries with a relatively thin layer that really turns it into an application.

"We've already added a `MonadUnliftIO` constraint on our functions" -- but you shouldn't have. Again, those classes are just duct tape on code that should really be refactored.

## Should we be that general?

Sure -- not much here.

## Do we need a transformer?

No. Lowering the transformer here totally makes sense.

## The m*n instance problem

Same. `Monad*` is totally an anti-pattern. Glad to hear it.

## Why not `ReaderT`?

So, I agreed that `ReaderT` doesn't buy us anything (and, in general, transformers should be seen as instance selectors) and `RIO` is a slightly-more-constrained newtype that is used instead in order to give us better instances.

But the same reasons to avoid `ReaderT` in the first place still apply to `RIO`.

## Some notes on the `Has` typeclasses

Can we get the superclass behavior with `:<:`?

```haskell
class element :<: container where
  get :: container -> element
  set :: container -> element -> container

-- | Every instance of `(:<:)` must form a lawful `Lens'`.
-- >>> view (has @Runner) someConfig :: Runner
has :: element :<: container => Lens' container element
has = lens get set

-- | covers `HasRunner Runner`, `HasConfig Config`, etc.
instance a :<: a where
  get = id
  set = const id

-- | reversed version of `class HasRunner env => HasConfig env`, which
--   simultaneously provides `HasRunner Config`, `HasRunner BuildConfig`, etc.
instance Config :<: a => Runner :<: a where
  get = configRunner . get
  set old new = old { configRunner = new }

instance BuildConfig :<: a => Config :<: a where
  get = buildConfigConfig . get
  set old new = old { buildConfigConfig = new }

instance EnvConfig :<: a => BuildConfig :<: a where
  get = envConfigBuildConfig . get
  set old new = old { envConfigBuildConfig = new }
```

Now you only need to define the highest instance in the tree -- and often not even that. E.g., we already have `Runner :<: EnvConfig` in the above definitions.

-   `Runner :<: EnvConfig` is implied by
-   `Config :<: EnvConfig`, which is implied by
-   `BuildConfig :<: EnvConfig`, which is implied by
-   `EnvConfig :<: EnvConfig`, which is a specialization of
-   `a :<: a`.

Have all the desirable behavior, fewer type classes, more laws, and fewer instances!

## How about pure code?

The last option here is what I strive for. Again, the rest are usually hints that you need to refactor. My overarching point is that you shouldn't stop doing it this way when `IO` shows up. It's even *more* important to not have stacks when `IO` is around.

## Using `RIO` in your application

Hopefully I've at least gotten you to question this approach. Again, sometimes you are saddled with stacks from other code and knowing how to manage them (like RIO does) is very useful. But I think in general there is way too much effort and information on how to manage stacks and not nearly enough on how and why to avoid building them in the first place.

Doing the right thing when you have a stack is important, but more often you should be asking how you can avoid the stack that you're looking at.

# Additional points

This section would be better as separate posts, but they exist in service of the points above, so they're here for the time being.

## <a id="boolean"></a>Eliminating boolean blindness

I think the simplest example of this is probably `filter`. It's type has a `Bool` in it: `filter :: (a -> Bool) -> [a] -> [a]`. There's only one boolean, but people still often forget what it means (i.e., does `True` mean "filter it out" or "keep it"?). We can answer the question decisively by changing the type to `filter :: (a -> Maybe b) -> [a] -> [b]`. The only thing we can keep in the list are `b`s (which may be the same as `a`s), but we need a `Just` to keep something. If we weakened the type just a bit, by replacing `b` with `a`, we still have a better _hint_ than the original `Bool` version, but it's still possible to implement it either way:
```haskell
-- filter _out_ the elements that return `Just`
filter test =
  foldr
  (\a acc -> case a of
      Nothing -> a : acc
	  Just _ -> acc)
  []
```
That implementation is counter-intuitive, but still legit. The version with `b` makes it impossible.

There are a few similar tricks like this for eliminating `Bool` (and other enum-like cases). Another one is that if you have some flag that triggers one side or the other of a branch,
```haskell
foo :: Bool -> W
foo myFlag =
  ...
  if myFlag
    then doA x y z
    else doB x y
  ...
```
you can give control to the caller by accepting a function instead of the `Bool`.
```haskell
foo :: (X -> Y -> Z -> V) -> Q
foo fn =
  ...
  fn x y z
  ...

doA, doB :: X -> Y -> Z -> V
doA x y z = ...
doB x y _z = ...
```

There are already blog posts on this ... should just link to one or two of those.

## <a id="logging"></a>Logging should be outside of your pure code

There are multiple kinds of logging, but they're often conflated. I feel like it's important to distinguish at least two kinds here (even though their solution is similar), because Stack's kind is not the one that I think comes up more often in practice.

First, some points that apply to both:
- you don't need any special monad for logging, just use `IO`;
- this doesn't mean you should introduce `IO` into pure functions in order to add logging.

Those two points may sound contradictory, so let's look at how we can apply them both.

### service debugging

Most logging is used in running distributed services to try to figure out what happened when, and make it possible to replicate some failure or other behavior. This is the more common case, I think, and so I'll cover it first.

Actually fixing a bug in a distributed service is something that should be done locally ... or at least on machines distinct from prod. In order to make that possible, you need to start by creating an environment where you can reliably replicate the bug. Then you can fix it, ensure it's fixed, and deploy a new version.

Logging mostly exists to make it possible to figure out _how_ to create an environment that matches the one where the error appeared.

Since pure functions don't admit `IO` (and therefore, don't admit logging as described in our earlier points), the "deepest" that we can log is the point at which we call a pure function. So, let's do that ... right around the call to a pure function, we log its inputs and its result.

Something goes wrong. We look at the logs and see that the result of the call to that pure function is not what we'd expect given the inputs. Well, darn ... now we know something is wrong _inside_ that function, but we have no logging information inside it, so we're stuck. Right?

Not at all! It's a pure function, which means that when we call it with those same arguments, we should get the same result. We quickly load the module into a REPL (make sure you have the same SHA as the deployed service!) and try it out. Sure enough, we get the same incorrect result. We've managed to perfectly replicate enough of the server environment (basically none) to reliably reproduce the bug. Now it's a simple matter of local debugging, with all our usual local tooling (including tracing). No need for adding any effects to our pure functions

### user information

The Stack use case for logging is different. It's providing information to the user (instead of the developer) to let them see what's happening along the way. They don't care what part of your code is pure or not, they just want to see various pieces of information as they're available. Often this information is not stuff that gets returned directly, but perhaps has already been processed into some other form by the time you're back at a level with `IO`. This means you're trying to exfiltrate some internal state so you can report it to the user.

The trick is to understand that if the state is truly internal, you shouldn't report it to the user. If it's something that you do want to report to the user, then there should be some part of the API that exposes it. So, the sweet spot for your definition should be the steps that you want to report to the user. E.g.,

```haskell
process :: Config -> IO A
process =
  logFinalRes . finalStep
  <=< logRes2 . step2
  <=< logRes1 . step1
```
where the `*step*` functions are all pure functions that encapsulate each thing that the application user can distinguish (and are all defined in some other library, or at least a separate module).

However, it's not uncommon that what you're trying to log is the steps of a _recursive function_. Which becomes much harder to extricate the logging from. If you know me, you know what's coming next ... this is where you want to decompose your recursive function into an algebra and use recursion schemes. So a pure
```haskell
cata evalTree myTree
```
becomes
```haskell
cataM (effectStep myLogger evalTree)
```
where we have
```haskell
-- | Takes a function that can log both the input and output of an algebra, the algebra itself, and returns a (now effectful) algebra.
effectStep :: Applicative m => (f a -> a -> m ()) -> (f a -> a) -> f a -> m a
effectStep eff alg input =
  let res = alg input
  in eff input res *> pure res
```
and `myLogger` is a function specific to your application.

But again, you don't always have this kind of control over the code you're using. You're sometimes stuck. And in those cases, yes, you need `IO` to (hopefully temporarily) leak into places it generally shouldn't in order to avoid long refactorings.

It's important to see those cases as technical debt and good "effect management" is a way to minimize that debt. But these should still be seen stop-gaps, not a desirable final architecture.

## How to refactor your stacks away

This is really a whole separate blog post, but when you see yourself with a monad stack, don't try to hide it, understand that the types are telling you to refactor your code. It's generally quite easy to get `IO` out of a stack:
- are you reading something from `IO`? Change it to a parameter
- are you writing to `IO`? Accumulate it in a data structure to be returned
- are you throwing exceptions? Use `Either` (especially with error accumulation ... which is yet _another_ blog post)

All of these things _should_ be pushed out from otherwise pure functions, and generally each of them should also have a separate function that is either `String -> Either E A` or `A -> String` to convert to/from some more structured data than whatever `IO` gives you.

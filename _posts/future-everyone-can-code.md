---
    layout: post
    publish: false
    title: Everyone Can Code (in the Future)
---

I spend a lot of time working on programming language design. Naturally, in my me-centric world, this means I want everyone to use my programming languages, so take the following with as much salt as needed to make it palatable.

I think programming is a skill that _everyone_ needs. If everyone could write Python, or Ruby, or whatever, that'd be great. But those all require way more understanding of programming than I think the large majority of people need. While the programming knowledge of the general public needs to increase, the level of abstraction provided by the programming languages they use needs to increase way more.

Being a Lisper, it has heavily influenced my thinking. Most lisps provide some form of syntactic abstraction (aka, macros – but not to be confused with the horrible, horrible "macros" provided by things like the C preprocessor). Part of the raison d’être of macros is that the language can be raised to the level of the problem – rather than having to express everything in at the level of the programming language.

And this is the solution, I think. I think of my "general purpose" languages as virtual machines – like the JVM or LLVM. The difference with my VM is that it is extremely high-level. The language itself manages memory and concurrency, and provides simple ways to describe distributed environments.

On top of this "VM" language (which comes with the libraries necessary to implement itself – ones for objects, networking, math, etc.), two types of things are built – often together. One is libraries, and the other (using appropriate libraries) is DSLs. 

DSLs are something you hear about a lot. Examples given are often things like `make`, which is a good example in one sense, and `rake`, which is a good example in a different sense. Rake is a good example, because it uses macros to extend the language with constructs to handle system building while still providing you with access to the underlying language (this means you can just let certain things "pass through" – like arithmetic – without having to reimplement them for your DSL, and also that expert users can do things that your DSL didn't take into account). Make is a good example, because it provides an entirely different syntax from C (the language in which it is implemented). However, it is hard to find examples where you have both complete freedom of syntax, and yet compositionality with other DSLs and the underlying language. To really take advantage of DSLs, we need both of these aspects.

But, why are DSLs important in the first place?

DSLs are helpful for everyone. For the general public, they allow them to express things in terminology and constructs that they are familiar with, without having to think about issues like error checking, thread-safety, and logic at the same level as professional programmers. For professional programmers, they are simply another level of abstraction and encapsulation.

For example, a DSL for airline workers might offer some construct for dealing with a flight-record, which is possibly stored as a file that needs to be opened and closed around its use. In a typical language (sans macros and first-class functions), you have no way to encapsulate the idea that you want to open the file, execute arbitrary code, then close the file. The closest you can get is something like:

	record = open_flight_record("F2Z4Q5");
	# do stuff with record
	close_flight_record(record);

First-class functions allow you to encapsulate that:

	with_flight_record("F2Z4Q5",
		{|record|
			# do stuff with record
		});

However, this still requires that you deal with general-purpose syntax – braces to indicate an anonymous function, pipes to define the arguments to the function, etc. A DSL could offer:

	with flight record F2Z4Q5
		# do stuff with flight record

The parser for a DSL can know that a flight record is always 6 alpha-numeric characters with no spaces, and could reasonably parse and handle identifiers with spaces.

There are languages (eg, [Katahdin http://www.chrisseaton.com/katahdin/]) that provide this sort of metaprogramming at the right level – where you can redefine the parser for the language very easily
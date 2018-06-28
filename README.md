# Luke's Programming Languages Zoo
Fine-grain (Small Step) implementations of common lambda calculi in Haskell. 

## Motivation
I've been studying the Foundations of Programming Languages, Semantics, and Type Theory. I decided to implement some of the common Lambda Calculi to solidify my understanding. 

The naming of this repo was inspired in part by Andrej Bauer's [plzoo](https://github.com/andrejbauer/plzoo).

One aim of the repo is to implement popular (functional) languages and extensions to portray how the theory translates into practice. 

The languages are written in Haskell and are intentionally simple. That is, they do not use advanced features of Haskell but rather minimal use of [type constructors](https://en.wikipedia.org/wiki/Algebraic_data_type), [recursion](https://www.google.co.uk/search?ei=MbCyWrPmOeWWgAbLr4TwCg&q=recursion&oq=recursion&gs_l=psy-ab.3..35i39k1l2j0i67k1l8.6142.7685.0.7949.10.7.0.0.0.0.543.543.5-1.1.0....0...1c.1.64.psy-ab..9.1.541.0...0.9MwCQCbDFwA), and [functional programming](https://learnxinyminutes.com/docs/haskell/). 

The intention here is to maximise your understanding of language design whilst minimising the need to understand Haskell. Of course it helps if you know it!

## Languages

1. [_ULC_](ULC/): Alonzo Church's [Untyped Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (Church Style)
2. [_SKI_](SKI/): Moses Schonfinkel's [SKI Combinator Calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus). In essence an (untyped) combinator calculus equivalent in [computational power](https://en.wikipedia.org/wiki/Turing_completeness) to ULC, but without abstraction.
3. [_STLC_](STLC/): Alonzo Church's [Simply-Typed Lambda Calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (Church) with one base type and function types
4. [_SystemT_](SystemT/): Kurt Godel's [System T](https://en.wikipedia.org/wiki/Dialectica_interpretation). In essence the STLC with [Nat](https://wiki.haskell.org/Peano_numbers) swapped out for the base type and [primitive recursion](https://www.quora.com/What-is-primitive-recursion) on Nats.
5. [_PCF_](PCF/): Gordon Plotkin's [Programming Computable Functions](http://www.cs.bham.ac.uk/~axj/pub/papers/Jung-2014-Teaching-denotational-semantics.pdf). In essence it's System T but using the [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus) for general recursion instead of primitive.
6. [_Mu_](Mu/): Michel Parigot's [Lambda-Mu](https://www.cs.ru.nl/~freek/courses/tt-2011/papers/parigot.pdf). In essence it's STLC with [continuations](https://en.wikipedia.org/wiki/Continuation) that don't rely on the reduction strategy used.
7. [_SystemF_](SystemF/): Jean Yves-Girard's [System F](https://en.wikipedia.org/wiki/System_F). In essence it's STLC with [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) built in.
8. [_Cata_](Cata/): In essence it's STLC with [inductive types](https://en.wikipedia.org/wiki/Inductive_type). 
9. [_Ana_](Ana/): In essence it's STLC with [coinductive types](https://en.wikipedia.org/wiki/Coinduction).
10. [_Sub_](Sub/): Benjamin Pierce's Lambda Calculus with Subtypes. In essence it's STLC with generalised records and [subtype polymorphism](https://en.wikipedia.org/wiki/Subtyping).

See each repo for details on installation/use and submit a PR if there's something you want to fix!

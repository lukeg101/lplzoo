# Luke's Programming Languages Zoo
Fine-grain (Small Step) implementations of common lambda calculi in Haskell. 

## Motivation
I've been studying the Foundations of Programming Languages, Semantics, and Type Theory. I decided to implement some of the common Lambda Calculi to solidify my understanding. 

The naming of this repo was inspired in part by Andrej Bauer's [plzoo](https://github.com/andrejbauer/plzoo).

One aim of the repo is to implement popular (functional) languages and extensions to portray how the theory translates into practice. 

The languages are written in Haskell and are intentionally simple. That is, they do not use advanced features of Haskell but rather minimal use of type constructors, recursion, and functional programming. 

The intention here is to maximise your understanding of language design whilst minimising the need to understand Haskell. Of course it helps if you know it!

## Languages

1. _ULC_: [Untyped Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (Church Style)
2. _STLC_: [Simply-Typed Lambda Calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (Church) with one base type and function types
3. _SystemT_: Kurt Godel's [System T](https://en.wikipedia.org/wiki/Dialectica_interpretation). In essence the STLC with Nat swapped out for the base type and primitive recursion on Nats.
4. _SystemF_: Jean Yves-Girard's [System F](https://en.wikipedia.org/wiki/System_F). In essence it's STLC with [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism) built in. 

See each repo for details on installation/use and submit a PR if there's something you want to fix!
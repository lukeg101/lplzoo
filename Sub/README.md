# Simply-Typed Lambda Calculus with Subtyping
Haskell implementation on Benjamin Pierce's Lambda Calculus with Subtyping. It has the features of STLC but with generalised records, a Unit type, and subtype polymorphism.

This strongly normalising calculus serves as a foundation for object-oriented  languages such as Java, Scala, and Python.

## Prerequisites
You need [Haskell](https://www.haskell.org/), this compiles with GHC 8.2.2 at least (Stack resolver: lts-11.0).

## To Build & Run

To compile and run do:
`ghc -O2 -o sub Main`
then run `./sub`

Alternatively to use the GHCi Interpreter do:
`ghci Main`
then type `main`

In either case you get something like the following:
```
Welcome to the Sub REPL
Type some terms or press Enter to leave.
>
```

Note: When run in GHCi, you don't have the luxury of escaped characters, backspace, delete etc...
Compile it using GHC if you need this.

## Examples 
Where you can then have some fun, try these examples:
- `{x=(), id=\x:A.x}` a record containing a unit `()` and the identity function for type `A` assigned to labels `x` and `id` respectively. 
- `\r:{a:A, b:B}.r` this is the identity function for 2 element records containing an `A` and a `B`.
- `(\r:{u:1}.r) {u={u=()}}` identity function for a singleton-record `{u:1}`, which due to subtyping will accept `{u={u=()}}`.
- `\r:{b:B}.r.b` a function taking a record `{b:B}` and projecting out the label `b` (submit a PR with a less confusing syntax).
- `(\f:1->1.f) (\g:A->A.g)` subtyping with arrow types.

The parser is smart enough to recognise λ and ⊤; so you can copy and paste from the output:
```
Welcome to the Sub REPL
Type some terms or press Enter to leave.
>   \r:{u:1}.r
=   λr:{u:⊤}.r
>   λr:{u:⊤}.r
=   λr:{u:⊤}.r
```
`>` denotes the REPL waiting for input, `=` means no reductions occurred (it's the same term), `~>` denotes one reduction, and `~>*` denotes 0 or more reductions (although in practice this is 1 or more due to `=`).

There is also a reduction tracer, which should print each reduction step. prefix any string with `'` in order to see the reductions:

```
>   '(\x:1.\f:1->1.f {id=\a:A.a, u=x}) () (\r:{u:1, g:A->1}.r.u)
~>  (λf:⊤->⊤.f {id=λa:A.a, u=()}) (λr:{u:⊤, g:A->⊤}.r.u)
~>  (λr:{u:⊤, g:A->⊤}.r.u) {id=λa:A.a, u=()}
~>  {id=λa:A.a, u=()}.u
~>  ()
```

There is also a typing mechanism, which should display the type or fail as usual.
```
>   t\r:{a:A, b:B}.r.b
{a:A, b:B}->B
>   t\r:{a:A, b:B}.r.c
Cannot Type Term: \r:{a:A, b:B}.r.c
```

Note: if you provide a non-normalizing term, the type checker will fail and reduction will not occur.

You can save terms for the life of the program with a `let` expression. Any time a saved variable appears in a term, it will be substituted for the saved term:
```
>   let f = (\x:1.\f:1->1.f {id=\a:A.a, u=x})
Saved term: λx:⊤.λf:⊤->⊤.f {id=λa:A.a, u=x}
>   let g = (\r:{u:1, g:A->1}.r.u)
Saved term: λr:{u:⊤, g:A->⊤}.r.u
>   f () g
~>* ()
```
Note: Consequently `let` and `=` are keywords, and so you cannot name variables with these.

Additionally we have type level `lett` statements that allow you to define and use types:
```
>   lett RECORD = {a:A, b:B, f:A->A, g:B->B}
Saved type: {a:A, b:B, f:A->A, g:B->B}
>   \r:RECORD.r
=   λr:{a:A, b:B, f:A->A, g:B->B}.r
```

This makes it easier to define both terms and types, but does not allow type level application (See Omega). `lett` is also a keyword.

## Syntax 

We base the language on the BNF for the typed calculus, with the addition of generalised records, record projections (using the record names as projections), the unit, and types for these terms:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&&space;()&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;A,B,C,...}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\top\\&space;&|&&space;\{\mathbf{\upsilon}\tt{:}\sigma,...\}&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\tau\,&space;{\tt&space;space}\,&space;\tau\\&space;&&space;|&space;&&space;\upsilon&space;\\&space;&|&&space;\{\upsilon=&space;\tau,...\}&space;\\&space;&|&\tau&space;.&space;\upsilon\\&space;&|&&space;()&space;\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\&space;&&\\&space;\sigma&space;&&space;::=&space;&&space;{\tt&space;A,B,C,...}\\&space;&&space;|&space;&&space;\sigma&space;\rightarrow&space;\sigma\\&space;&|&&space;\top\\&space;&|&&space;\{\mathbf{\upsilon}\tt{:}\sigma,...\}&space;\end{matrix}" title="\begin{matrix} \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \tau\, {\tt space}\, \tau\\ & | & \upsilon \\ &|& \{\upsilon= \tau,...\} \\ &|&\tau . \upsilon\\ &|& () \\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\ &&\\ \sigma & ::= & {\tt A,B,C,...}\\ & | & \sigma \rightarrow \sigma\\ &|& \top\\ &|& \{\mathbf{\upsilon}\tt{:}\sigma,...\} \end{matrix}" /></a>

However we adopt standard bracketing conventions to eliminate ambiguity in the parser. Concretely, the parser implements the non-ambiguous grammar for terms as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;()&space;\\&space;&|&&space;\{\eta\}\\&space;&|&&space;\upsilon.\upsilon\\&space;&|&&space;\{\eta\}.\upsilon\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\\&space;&&\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;&&\\&space;\mathbf{\tau}&&space;::=&space;&&space;\lambda&space;\mathbf{\upsilon}\tt{:}\sigma&space;.&space;\mathbf{\tau}\\&space;&&space;|&space;&&space;\alpha\\&space;&&\\&space;\alpha&space;&&space;::=&space;&&space;\beta&space;\\&space;&|&space;&\mathbf{\alpha\,&space;\tt{space}\,&space;\beta}&space;\\&space;&&\\&space;\beta&space;&&space;::=&space;&&space;\tt{(}\tau&space;\tt{)}\\&space;&|&&space;\upsilon&space;\\&space;&|&&space;()&space;\\&space;&|&&space;\{\eta\}\\&space;&|&&space;\upsilon.\upsilon\\&space;&|&&space;\{\eta\}.\upsilon\\&space;&&\\&space;\upsilon&space;&&space;::=&space;&&space;\tt{a}&space;|&space;\tt{b}&space;|&space;\tt{c}&space;|&space;...&space;|&space;\tt{aa}&space;|&space;...&space;\\\&space;&&\\&space;\end{matrix}" title="\begin{matrix} &&\\ \mathbf{\tau}& ::= & \lambda \mathbf{\upsilon}\tt{:}\sigma . \mathbf{\tau}\\ & | & \alpha\\ &&\\ \alpha & ::= & \beta \\ &| &\mathbf{\alpha\, \tt{space}\, \beta} \\ &&\\ \beta & ::= & \tt{(}\tau \tt{)}\\ &|& \upsilon \\ &|& () \\ &|& \{\eta\}\\ &|& \upsilon.\upsilon\\ &|& \{\eta\}.\upsilon\\ &&\\ \upsilon & ::= & \tt{a} | \tt{b} | \tt{c} | ... | \tt{aa} | ... \\\ &&\\ \end{matrix}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\eta&space;&&space;::=&space;&&space;\upsilon&space;=&space;\tau\\&space;&|&&space;\upsilon&space;=&space;\tau&space;,&space;\eta&space;\\&space;\end{matrix}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\begin{matrix}&space;\eta&space;&&space;::=&space;&&space;\upsilon&space;=&space;\tau\\&space;&|&&space;\upsilon&space;=&space;\tau&space;,&space;\eta&space;\\&space;\end{matrix}" title="\begin{matrix} \eta & ::= & \upsilon = \tau\\ &|& \upsilon = \tau , \eta \\ \end{matrix}" /></a>

and types:

<a href="http://www.codecogs.com/eqnedit.php?latex=\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\rightarrow&space;\sigma\\&space;\\&space;\gamma&space;&::=&&space;A,B,C,...\\&space;&|&&space;(\sigma)\\&space;&|&\top\\&space;&|&&space;\{\varrho\}&space;\\&space;\\&space;\varrho&space;&&space;::=&space;&&space;\upsilon&space;:&space;\sigma\\&space;&|&&space;\upsilon&space;:&space;\sigma&space;,&space;\varrho&space;\\&space;\end{matrix}" target="_blank"><img src="http://latex.codecogs.com/gif.latex?\begin{matrix}&space;\sigma&space;&&space;::=&space;&&space;\gamma&space;\\&space;&&space;|&space;&&space;\gamma&space;\rightarrow&space;\sigma\\&space;\\&space;\gamma&space;&::=&&space;A,B,C,...\\&space;&|&&space;(\sigma)\\&space;&|&\top\\&space;&|&&space;\{\varrho\}&space;\\&space;\\&space;\varrho&space;&&space;::=&space;&&space;\upsilon&space;:&space;\sigma\\&space;&|&&space;\upsilon&space;:&space;\sigma&space;,&space;\varrho&space;\\&space;\end{matrix}" title="\begin{matrix} \sigma & ::= & \gamma \\ & | & \gamma \rightarrow \sigma\\ \\ \gamma &::=& A,B,C,...\\ &|& (\sigma)\\ &|&\top\\ &|& \{\varrho\} \\ \\ \varrho & ::= & \upsilon : \sigma\\ &|& \upsilon : \sigma , \varrho \\ \end{matrix}" /></a>

Some notes about the syntax:

- The above syntax only covers the core calculus, and not the repl extensions (such as let bindings above). The extensions are simply added on in the repl.
- Variables are strings (excluding numbers), as this is isomorphic to a whiteboard treatment and hence the most familiar.
- Units are largely uninteresting, but can be formed as `()` like in Haskell, and typed like `1` or (or `⊤`).
- Records are generalised products formed as comma-separated sequence of assignments of terms to labels `t=v`, nested inside curly braces `{u=(), ida=\a:A.a}`. Record types are comma-separated assignments of typings to labels `t:v`, nested inside curly braces (such as `{1:⊤, f:A->A}`).
- Despite using subtyping, the subtyping relation is not part of the syntax or language itself but rather a feature used during typechecking. As such it is not featured in the grammars.
- Types range over upper-case characters `A,B,C...`, nested arrow types: `T -> T`, the unit type `()`, and generalised record types `{a:A,f:B->A,...,c:C}`.
- Arrows associate to the right so that `T -> T -> T` is the same as `T -> (T -> T)` but not `((T -> T) -> T)`. 
- Nested terms don't need brackets: `\a:A.\b:B. b` unless enforcing application on the right. Whitespace does not matter `\r:{a:A, b:B}.    r` unless it is between application where you need at least one space.
- To quit use `Ctrl+C` or whatever your machine uses to interrupt computations.

## Semantics

The semantics implements beta-reduction on terms and alpha-equivalence as the `Eq` instance of `STerm`. The semantics are the same as STLC with the addition of subtype polymorphism with augments the typing relation with a subtyping relation `<`. We reformulate the semantics as [typing judgements](https://existentialtype.wordpress.com/2011/03/27/the-holy-trinity/):

We have typing rules for the Unit type (otherwise known as Top):

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma\vdash&space;()&space;:&space;\top}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma\vdash&space;()&space;:&space;\top}" title="\overline{\Gamma\vdash () : \top}" /></a>

The subtyping relation is a reflexive and transitive relation `<` defined as follows:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{S&space;<&space;S}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{S&space;<&space;S}" title="\overline{S < S}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{A&space;<&space;B\quad&space;B&space;<&space;C}{A&space;<&space;C}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{A&space;<&space;B\quad&space;B&space;<&space;C}{A&space;<&space;C}" title="\frac{A < B\quad B < C}{A < C}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{S&space;<&space;\top}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{S&space;<&space;\top}" title="\overline{S < \top}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{T_1&space;<&space;S_1&space;\quad&space;S_2&space;<&space;T_2}{S_1&space;\rightarrow&space;S_2&space;<&space;T_1&space;\rightarrow&space;T_2}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{T_1&space;<&space;S_1&space;\quad&space;S_2&space;<&space;T_2}{S_1&space;\rightarrow&space;S_2&space;<&space;T_1&space;\rightarrow&space;T_2}" title="\frac{T_1 < S_1 \quad S_2 < T_2}{S_1 \rightarrow S_2 < T_1 \rightarrow T_2}" /></a>

These rules denote reflexivity, transitivity, that `()` is the _supertype_ of all types, and the arrow-subtyping rule for functions. The standard typing rules from STLC are then influenced by this, but structurally are the same beyond a type substitution rule:

Variables:

<a href="https://www.codecogs.com/eqnedit.php?latex=\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\overline{\Gamma&space;\vdash&space;x:T},\quad&space;\mbox{(if&space;$x:T&space;\in&space;\Gamma$)}" title="\overline{\Gamma \vdash x:T},\quad \mbox{(if $x:T \in \Gamma$)}" /></a>

for abstractions:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;,x:A&space;\vdash&space;t:B}{\Gamma&space;\vdash&space;(\lambda&space;x&space;:&space;A.&space;t)&space;:&space;B&space;}" title="\frac{\Gamma ,x:A \vdash t:B}{\Gamma \vdash (\lambda x : A. t) : B }" /></a>

for application:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;f&space;:&space;A&space;\Rightarrow&space;B\quad&space;\Gamma&space;\vdash&space;x&space;:&space;A}{\Gamma&space;\vdash&space;(f&space;x)&space;:&space;B}" title="\frac{\Gamma \vdash f : A \Rightarrow B\quad \Gamma \vdash x : A}{\Gamma \vdash (f x) : B}" /></a>

the subtyping substitution rule (implicit in typing, not explicitly used):

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\Gamma&space;\vdash&space;t:A\quad&space;A&space;<&space;B}{\Gamma&space;\vdash&space;t:B&space;}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\Gamma&space;\vdash&space;t:A\quad&space;A&space;<&space;B}{\Gamma&space;\vdash&space;t:B&space;}" title="\frac{\Gamma \vdash t:A\quad A < B}{\Gamma \vdash t:B }" /></a>

and the reduction relation (adopted from STLC):

<a href="https://www.codecogs.com/eqnedit.php?latex=(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" target="_blank"><img src="https://latex.codecogs.com/gif.latex?(\lambda&space;x&space;:&space;T&space;.&space;M)N&space;\rightsquigarrow&space;M&space;[x&space;:=&space;N]" title="(\lambda x : T . M)N \rightsquigarrow M [x := N]" /></a>

Next, there is a special elimination rule for records:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t_i&space;:&space;T_i}{\forall&space;i.\,\Gamma&space;\vdash&space;\{l_i=t_i&space;\}:\{l_i:T_i\}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t_i&space;:&space;T_i}{\forall&space;i.\,\Gamma&space;\vdash&space;\{l_i=t_i&space;\}:\{l_i:T_i\}}" title="\frac{\forall i .\,\Gamma \vdash t_i : T_i}{\forall i.\,\Gamma \vdash \{l_i=t_i \}:\{l_i:T_i\}}" /></a>

with sensible structural (internal) reduction rules for terms inside records. Lastly we have rules for record projections:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t&space;:&space;\{l_i:T_i\}}{\forall&space;i.\,\Gamma&space;\vdash&space;t.l_i&space;:&space;T_i}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\forall&space;i&space;.\,\Gamma&space;\vdash&space;t&space;:&space;\{l_i:T_i\}}{\forall&space;i.\,\Gamma&space;\vdash&space;t.l_i&space;:&space;T_i}" title="\frac{\forall i .\,\Gamma \vdash t : \{l_i:T_i\}}{\forall i.\,\Gamma \vdash t.l_i : T_i}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{t&space;\rightsquigarrow&space;t'}{t.l&space;\rightsquigarrow&space;t'.l}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{t&space;\rightsquigarrow&space;t'}{t.l&space;\rightsquigarrow&space;t'.l}" title="\frac{t \rightsquigarrow t'}{t.l \rightsquigarrow t'.l}" /></a>

<a href="https://www.codecogs.com/eqnedit.php?latex=\{l_i=t_i\}.l&space;\rightsquigarrow&space;t_i" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\{l_i=t_i\}.l&space;\rightsquigarrow&space;t_i" title="\{l_i=t_i\}.l \rightsquigarrow t_i" /></a>

with [width](https://www.cs.cornell.edu/courses/cs4110/2012fa/lectures/lecture24.pdf) subtyping rules for records the subtype record can have more fields than its supertype record as long as the common fields match on label and type):

<a href="https://www.codecogs.com/eqnedit.php?latex=\{&space;l_i&space;:&space;T_i^{\forall&space;i&space;\in&space;1..n&plus;k}&space;\}&space;<&space;\{&space;l_i&space;:&space;T_i^{\forall&space;i&space;\in&space;1..n}&space;\}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\{&space;l_i&space;:&space;T_i^{\forall&space;i&space;\in&space;1..n&plus;k}&space;\}&space;<&space;\{&space;l_i&space;:&space;T_i^{\forall&space;i&space;\in&space;1..n}&space;\}" title="\{ l_i : T_i^{\forall i \in 1..n+k} \} < \{ l_i : T_i^{\forall i \in 1..n} \}" /></a>

Which we extend width record depth subtyping in which common fields in records need to match on labels but their types need only to form the subtyping relation, rather than stricter width subtyping above:

<a href="https://www.codecogs.com/eqnedit.php?latex=\frac{\forall&space;i.&space;S_i&space;<&space;T_i}{\{l_i:S_i\}&space;<&space;\{l_i&space;:&space;T_i\}}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\frac{\forall&space;i.&space;S_i&space;<&space;T_i}{\{l_i:S_i\}&space;<&space;\{l_i&space;:&space;T_i\}}" title="\frac{\forall i. S_i < T_i}{\{l_i:S_i\} < \{l_i : T_i\}}" /></a>

- We do structural subtyping here so that the structure of a type determines whether it's a subtype of another type. The alternative is nominal subtyping where terms are subtypes when defined in a certain way (like how classes extend supertypes in Java).
- Field ordering in records does not matter, enabling _permutation_ subtyping.
- The subtyping relation is not explicit in the language but rather done at typechecking time and so any terms using `<` are for demonstration and are not parsable in Sub.
- Records are typeable only if all of their subterms are typeable and all of the labels are unique. We leverage the STLC typing rules and the subtyping relation to ensure record fields are typeable.
- Projections are typeable only if it is applied to a term which is a well-typed record and the projection label (`x` in `{x=()}.x`) explicitly matches a label in that record.
- Units are considered to be equivalent to _Object_ in conventional object-oriented languages and hence a function taking a `()` will take anything, as everything is a subtype of Object: `(\x:1.x) (\a:A.a)`. We should mention that languages like Java might include additional baggage like [hashCode](https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html), our treatment is simpler as we don't need this to demonstrate how subtyping works. The subtyping relation therefore states that all types are subtypes of units `():1`.
- All base types are on the same level and hence `A < B` for arbitrary `A != B` is false but `A < A` is true (due to the reflexivity of `<`).
- For arrow types `f:S1 -> S2` and `g:T1 -> T2` we require that `T1 < S1` and `S2 < T2` if `f < g`. The first is needed as `g` expects a type `T1` and `f` provides a type `S1` with a domain (input space) that may supercede `T1`. The second is because the codomain (result) of `f` should be a subtype of `g` if we are going to use `S2` anywhere `T2` would be expected. This is demonstrated by the term `(\f:1->1.f) (\g:A->A.g)`
- This implementation follows a [small-step](https://cs.stackexchange.com/questions/43294/difference-between-small-and-big-step-operational-semantics) operational semantics and Berendregt's [variable convention](https://cs.stackexchange.com/questions/69323/barendregts-variable-convention-what-does-it-mean) (see `substitution` in Sub.hs). The variable convention is adopted for both types and terms.
- Reductions include the one-step reduction (see `reduce1` in Sub.hs), the many-step reduction (see `reduce` in Sub.hs).

## Other Implementation Details
- Sub.hs contains the Haskell implementation of the calculus, including substitution, subtyping, reduction, and other useful things.
- Parser.hs contains the monadic parser combinators needed to parse input strings into typed-term ASTs for the calculus.
- Repl.hs contains a simple read-eval-print loop which hooks into main, and into the parser.
- Main.hs is needed for GHC to compile without any flags, it also invokes the repl.

For contributions, see the project to-do list or submit a PR with something you think it needs.



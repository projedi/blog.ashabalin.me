---
title: Heterogenous equality
---

Dependently typed languages have two types of equality: propositional and decidable.

Decidable equality is just like equality from all the other programming languages:
we have a function that checks its 2 arguments for equality at *run time*.

For expressing propositonal equality we have a type that checks its 2 arguments
for equality at *compile time*.

This is how Agda standard library defines the latter
([source code](https://github.com/agda/agda-stdlib/blob/v0.9/src/Relation/Binary/Core.agda#L151)):
```agda
data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}
```

Now let's try to prove a simple property of vectors: `xs ++ [] = xs`:
```{.agda include="Agda1.agda"}
```
And we immediatly fail with an error:
```
n != n .Data.Nat.+ 0 of type .Data.Nat.ℕ
when checking that the expression xs has type
Vec A (n .Data.Nat.+ 0)
```
The reason for it is simple: `xs ++ []` has type `Vec A (n + 0)` while
`xs` has type `Vec A n` and since `n + 0` does not reduce to `n` typechecker
has no idea that these two types are identical.  There does not seem to be a
way to introduce a proof of `n + 0 ≡ n` into the typechecker here.

But let's look at what Idris does here. Here's a definition (I am lying, it is
actually hardcoded in the compiler
[here](https://github.com/idris-lang/Idris-dev/blob/v0.9.15.1/src/Idris/AbsSyntaxTree.hs#L1098)):
```idris
data (=) : (x : a) -> (y : b) -> Type where
  Refl : x = x
```
Notice that two arguments of `(=)` are of different types! This is because Idris has
*heterogenous* equality instead of *homogenous* like Agda. Actually it is somewhat mixed:
Idris will try to use homogenous version first (i.e. when `b = a`) and if it fails to typecheck
will fall back to the heterogenous one. I do not know why it does that.

We can now write this:
```{.idris include="Idris1.idr"}
```
A thing to note is that we had to write a helper `cons_cong` instead of using `cong (::)`.
Here is why: `cong : (f : a -> b) -> (x = y) -> (f x = f y)`. `f` has to be the same. That is,
`(::)` has to be the same but it is not because on the left side of `(=)`
`(::) : a -> Vect (n + 0) a -> Vect (S (n + 0)) a` and on the right side
`(::) : a -> Vect n a -> Vect (S n) a`. `cons_cong` lifts this requirement.

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

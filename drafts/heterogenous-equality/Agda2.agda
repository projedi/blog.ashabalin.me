module Agda2 where

open import Data.Vec
open import Data.Vec.Equality
open import Relation.Binary.Core

open PropositionalEquality

lemma-[]-right-++-identity : ∀ {l n} {A : Set l} (xs : Vec A n) → xs ++ [] ≈ xs
lemma-[]-right-++-identity [] = []-cong
lemma-[]-right-++-identity (x ∷ xs) = _≡_.refl ∷-cong (lemma-[]-right-++-identity xs)

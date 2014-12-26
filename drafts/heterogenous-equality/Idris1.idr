module Idris1

lemma_nil_right_concat_identity : (xs : Vect n a) -> xs ++ [] = xs
lemma_nil_right_concat_identity [] = Refl
lemma_nil_right_concat_identity (x :: xs) = cons_cong _ _ _ (lemma_nil_right_concat_identity xs)
 where cons_cong : (x : a) -> (xs : Vect n a) -> (ys : Vect m a) -> (xs = ys) -> (x :: xs = x :: ys)
       cons_cong _ _ _ Refl = Refl


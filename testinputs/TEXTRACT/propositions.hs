{-# LINE 58 "propvrb.tex" #-}
module Propositions where
  
{-# LINE 257 "propvrb.tex" #-}

  import Prelude(Eq, String, Show, ShowS, Int, 
                 map, (.), (++), foldr, take, unlines, (>=), (+), (*), ($), 
                 showParen, showString, showsPrec, show)
  import List

{-# LINE 266 "propvrb.tex" #-}
 -- A supplementary deferred section
 
{-# LINE 61 "propvrb.tex" #-}

  data Prop = Atom Atomic         -- a, b ...
            | Not  Prop           -- not p
            | Prop `And` Prop     -- p ∧ q
            | Prop `Or`  Prop     -- p ∨ q
            | Prop `Imp` Prop     -- p ⇒ q
            | Prop `Iff` Prop     -- p ⇔ q
            deriving (Eq)
{-# LINE 81 "propvrb.tex" #-}
  infixl 9 ∧;  (∧)  = And
  infixl 8 ∨;  (∨)  = Or
  infixr 7 ⇒; (⇒)  = Imp
  infixl 6 ⇔; (⇔)  = Iff  
  not = Not
{-# LINE 93 "propvrb.tex" #-}
  type Atomic = String
  [a,b,c,d,p,q,r,s] = map (Atom . (:"")) "abcdpqrs"
{-# LINE 106 "propvrb.tex" #-}
  instance (Show Prop) where showsPrec = showProp  
  showProp::  Int -> Prop -> ShowS
  showProp d (Atom a)    = showString a
  showProp d (Not  p)    = showString "not " . showProp 10 p
  showProp d (p `And` q) = showInfix d 9 9 " ∧ "     p q
  showProp d (p `Or` q)  = showInfix d 8 8 " ∨ "     p q
  showProp d (p `Imp` q) = showInfix d 7 6 " ⇒ "    p q
  showProp d (p `Iff` q) = showInfix d 5 5 " ⇔ "    p q
{-# LINE 123 "propvrb.tex" #-}
  showInfix:: Int -> Int -> Int -> String -> Prop -> Prop -> ShowS
  showInfix d d' d'' op l r  =  
            showParen (d>=d') $ 
            showProp  d' l   .
            showString op    . 
            showProp  d'' r
{-# LINE 134 "propvrb.tex" #-}
  p0 = (a ⇒ b) ⇒ c
  p1 = a ⇒ b ⇒ c
  p2 = a ∧ b ⇒ c ∧ d
  p3 = not a  ∧  b  ⇒  not c  ∧  d
  p4 = (a ⇒ (b ⇒ c)) ⇒ ((a ⇒ b) ⇒ c)
  p5 = not p4
{-# LINE 159 "propvrb.tex" #-}
  newtype Sequent = [Prop] :|- Prop
  infix 1 |-
  infix 1 :|-
  hyps |- conc = hyps :|- conc
      
  instance (Show Sequent) where 
     show (hyps :|- conc) = showHyps hyps ++ " :|- " ++ show conc
     
  data    Proof   = Proven [Proof] Sequent Rule
                  | Gap
                  
  instance (Show Proof) where
     showsPrec = showProof 
     
  showHyps []     = ""
  showHyps [h]    = show h
  showHyps (h:hs) = show h ++  ", " ++ showHyps hs
  
  showProof :: Int -> Proof -> ShowS
  showProof level Gap = indentBy level . showString "..."
  showProof level (Proven subproofs seq rule) =
            foldAll showSubproof subproofs .
            indentBy level                 .
            showsPrec 0 seq                .
            showString " by "              .
            showString rule
            where 
            showSubproof proof s = showProof (level+1) proof ("\n" ++ s)
            foldAll f xs ys = foldr f ys xs         
                  
  indentBy:: Int -> ShowS
  indentBy level s = take (2*level) spaces ++ s
  
  spaces:: String
  spaces = ' ':spaces 
  
  type Rule = String

  goal1 = [p ⇒ (q ⇒ r)] :|- p ⇒ r

  
{-# LINE 204 "propvrb.tex" #-}
{-
        RULE  "⇒⊢"
        FROM  Γ       ⊢ A
        AND   Γ, B    ⊢ C
        INFER Γ, A⇒B ⊢ C 
        
        RULE  "⊢⇒"
        FROM  Γ, A ⊢ B
        INFER Γ    ⊢ A⇒B
-}
{-# LINE 216 "propvrb.tex" #-}
  (∪) = List.union
  (∩) = List.intersect
  (ε)  = List.member
  
  
{-# LINE 272 "propvrb.tex" #-}
 -- A final comment
 

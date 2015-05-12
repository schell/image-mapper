{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
module UI.Diff where

import UI.Types
import Data.Typeable
import Data.Generic.Diff

type a $ b = Cons a b
infixr 1 $

data UIFamily t ts where
    --Float'    :: Float       -> UIFamily Float Nil
    --Int'      :: Int         -> UIFamily Int Nil
    --V2'       :: V2 Float    -> UIFamily (V2 Float) Nil
    --V4'       :: V4 Float    -> UIFamily (V4 Float) Nil
    --String'   :: String      -> UIFamily String Nil
    --AABB'     :: UIFamily AABB (V2 Float $ V2 Float $ Nil)
    UICons'   :: UIFamily [UserInterface] (UserInterface $ [UserInterface] $ Nil)
    UINil'    :: UIFamily [UserInterface] Nil
    UIBranch' :: UIFamily (UserInterface) (Uid $ UITransform $ [UserInterface] $ Nil)
    UILeaf'   :: UIFamily (UserInterface) (Uid $ UITransform $ UIElement $ Nil)
    Uid'      :: Uid         -> UIFamily Uid Nil
    UITfrm'   :: UITransform -> UIFamily UITransform Nil--(V2 Float $ V2 Float $ V2 Float $ Float $ Nil)
    UILabel'  :: UIElement -> UIFamily UIElement Nil --(String $ (V4 Float) $ (V4 Float) $ Nil)
    deriving (Typeable)

($$) :: x -> xs -> Cons x xs
a $$ b = CCons a b
infixr 1 $$

pattern a :$ b = CCons a b
infixr 1 :$

instance Family UIFamily where
    {-
    decEq (Float' x) (Float' y) | x == y     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (Int' x)   (Int' y)   | x == y     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (V2'  v)   (V2' w)    | v == w     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (V4'  v)   (V4' w)    | v == w     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (String' s)(String' t)| s == t     = Just (Refl, Refl)
                                | otherwise  = Nothing
    -}
    decEq UICons'  UICons'               = Just (Refl, Refl)
    decEq UINil'   UINil'                = Just (Refl, Refl)
    --decEq AABB'    AABB'                 = Just (Refl, Refl)
    decEq (Uid' u)   (Uid' d)        | u == d     = Just (Refl, Refl)
                                     | otherwise  = Nothing
    decEq (UILabel' u) (UILabel' u') | u == u' = Just (Refl, Refl)
                                     | otherwise = Nothing
    decEq (UITfrm' t) (UITfrm' t')   | t == t' = Just (Refl, Refl)
                                     | otherwise = Nothing
    decEq UIBranch' UIBranch'            = Just (Refl, Refl)
    decEq UILeaf' UILeaf'                = Just (Refl, Refl)
    decEq _ _ = Nothing
    {-
    fields (Float' _)  _          = Just CNil
    fields (Int' _)    _          = Just CNil
    fields (V2' _)     _          = Just CNil
    fields (V4' _)     _          = Just CNil
    fields (String' _) _          = Just CNil
    -}
    fields UICons'   (e:es)       = Just (e $$ es $$ CNil)
    fields UINil'    []           = Just CNil
    --fields AABB' (AABB c s)       = Just (c $$ s $$ CNil)
    fields (Uid' _)    _          = Just CNil
    fields (UILabel' _ ) _ = Just CNil
    fields UIBranch' (UIBranch u t ts) = Just (u $$ t $$ ts $$ CNil)
    fields UILeaf' (UILeaf u t e) = Just (u $$ t $$ e $$ CNil)
    fields (UITfrm' _) _ = Just CNil
    fields _ _ = Nothing
    {-
    apply (Float' x) CNil = x
    apply (Int' i)   CNil = i
    apply (V2' v)    CNil = v
    apply (V4' v)    CNil = v
    apply (String' u)CNil = u
    -}
    --apply AABB' (c :$ s :$ CNil) = AABB c s
    apply UICons' (e :$ es :$ CNil) = e : es
    apply UINil'   CNil = []
    apply UIBranch' (u :$ t :$ ts :$ CNil) = UIBranch u t ts
    apply UILeaf' (u :$ t :$ e :$ CNil) = UILeaf u t e
    apply (Uid' u)   CNil = u
    apply (UILabel' u) CNil = u
    apply (UITfrm' t) CNil = t
    {-
    string (Float' x) = show x
    string (Int' i)   = show i
    string (V2' v)    = show v
    string (V4' v)    = show v
    string (String' s)= show s
    -}
    --string AABB'      = "AABB"
    string UICons'    = "UITreeCons"
    string UINil'     = "UITreeNil"
    string UIBranch'  = "UIBranch"
    string UILeaf'    = "UILeaf"
    string (Uid' u)   = show u
    string (UITfrm' t) = show t
    string (UILabel' u) = show u
{-
instance Type UIFamily Float where
    constructors = [Abstr Float']

instance Type UIFamily Int where
    constructors = [Abstr Int']

instance Type UIFamily (V2 Float) where
    constructors = [Abstr V2']

instance Type UIFamily (V4 Float) where
    constructors = [Abstr V4']

instance Type UIFamily String where
    constructors = [Abstr String']

instance Type UIFamily AABB where
    constructors = [Concr AABB']
-}

instance Type UIFamily UserInterface where
    constructors = [Concr UIBranch', Concr UILeaf']

instance Type UIFamily [UserInterface] where
    constructors = [Concr UICons', Concr UINil']

instance Type UIFamily Uid where
    constructors = [Abstr Uid']

instance Type UIFamily UITransform where
    constructors = [Abstr UITfrm']

instance Type UIFamily UIElement where
    constructors = [Abstr UILabel']

diffElement :: UserInterface -> UserInterface -> EditScript UIFamily UserInterface UserInterface
diffElement = (compress .) . diff

showEditScript :: EditScriptL f x y -> String
showEditScript (Ins c d) = "(Ins " ++ string c ++ " " ++ showEditScript d ++ ")"
showEditScript (Del c d) = "(Del " ++ string c ++ " " ++ showEditScript d ++ ")"
showEditScript (Cpy c d) = "(Cpy " ++ string c ++ " " ++ showEditScript d ++ ")"
showEditScript (CpyTree d) = "(CpyTree " ++ showEditScript d ++ ")"
showEditScript End = "End"


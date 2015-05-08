{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
module UI.Diff where

import UI.Types
import Gelatin.Core.Render hiding (trace)
import Data.Typeable
import Data.Generic.Diff

type UIEditScript  = EditScript UIFamily [UIElement] [UIElement]

type a $ b = Cons a b
infixr 1 $

data UIFamily :: * -> * -> * where
    Float'    :: Float       -> UIFamily Float Nil
    Int'      :: Int         -> UIFamily Int Nil
    V2'       :: V2 Float    -> UIFamily (V2 Float) Nil
    V4'       :: V4 Float    -> UIFamily (V4 Float) Nil
    Uid'      :: Uid         -> UIFamily Uid Nil
    String'   :: String      -> UIFamily String Nil
    --UIEls'    :: [UIElement] -> UIFamily [UIElement] Nil
    UIElCons' :: UIFamily [UIElement] (UIElement $ [UIElement] $ Nil)
    UIElNil'  :: UIFamily [UIElement] Nil
    AABB'     :: UIFamily AABB (V2 Float $ V2 Float $ Nil)
    UI'       :: UIFamily UI (Uid $ V2 Float $ V2 Float $ V2 Float $ Float $ Nil)
    UICont'   :: UIFamily UIElement (UI $ [UIElement] $ Nil)
    UILabel'  :: UI -> UIFamily UIElement (UI $ String $ (V4 Float) $ (V4 Float) $ Nil)
    deriving (Typeable)

data Update = UILabelUpdate
            | UIContainerUpdate
            | UIElementList

updateParams :: Update -> Int
updateParams UIElementList = 1
updateParams UILabelUpdate = 4
updateParams UIContainerUpdate = 2

($$) :: x -> xs -> Cons x xs
a $$ b = CCons a b
infixr 1 $$

pattern a :$ b = CCons a b
infixr 1 :$

instance Family UIFamily where
    decEq (Float' x) (Float' y) | x == y     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (Int' x)   (Int' y)   | x == y     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (V2'  v)   (V2' w)    | v == w     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (V4'  v)   (V4' w)    | v == w     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (Uid' u)   (Uid' d)   | u == d     = Just (Refl, Refl)
                                | otherwise  = Nothing
    decEq (String' s)(String' t)| s == t     = Just (Refl, Refl)
                                | otherwise  = Nothing
    --decEq (UIEls' es)(UIEls' es')| es == es' = Just (Refl, Refl)
    --                             | otherwise = Nothing
    decEq UIElCons'  UIElCons'               = Just (Refl, Refl)
    decEq UIElNil'   UIElNil'                = Just (Refl, Refl)
    decEq AABB'      AABB'                   = Just (Refl, Refl)
    decEq UI'        UI'                     = Just (Refl, Refl)
    decEq UICont'    UICont'                 = Just (Refl, Refl)
    decEq (UILabel' u)   (UILabel' u') | u == u' = Just (Refl, Refl)
                                       | otherwise = Nothing
    decEq _ _ = Nothing

    fields (Float' _)  _          = Just CNil
    fields (Int' _)    _          = Just CNil
    fields (V2' _)     _          = Just CNil
    fields (V4' _)     _          = Just CNil
    fields (Uid' _)    _          = Just CNil
    fields (String' _) _          = Just CNil
    --fields (UIEls' _)  _          = Just CNil
    fields UIElCons'   (e:es)     = Just (e $$ es $$ CNil)
    fields UIElNil'    []         = Just CNil
    fields AABB' (AABB c s)       = Just (c $$ s $$ CNil)
    fields UI' (UI u p sz sc r)   = Just (u $$ p $$ sz $$ sc $$ r $$ CNil)
    fields UICont' (UIContainer ui els) = Just (ui $$ els $$ CNil)
    fields (UILabel' _) (UILabel u s tc bc) = Just (u $$ s $$ tc $$ bc $$ CNil)
    fields _ _ = Nothing

    apply (Float' x) CNil = x
    apply (Int' i)   CNil = i
    apply (V2' v)    CNil = v
    apply (V4' v)    CNil = v
    apply (Uid' u)   CNil = u
    apply (String' u)CNil = u
    apply AABB' (c :$ s :$ CNil) = AABB c s
    apply UI' (u :$ p :$ sz :$ sc :$ r :$ CNil) = UI u p sz sc r
    --apply (UIEls' es) CNil = es
    apply UIElCons' (e :$ es :$ CNil) = e : es
    apply UIElNil'   CNil = []
    apply UICont' (ui :$ els :$ CNil) = UIContainer ui els
    apply (UILabel' _) (ui :$ s :$ tc :$ bc :$ CNil) = UILabel ui s tc bc

    string (Float' x) = show x
    string (Int' i)   = show i
    string (V2' v)    = show v
    string (V4' v)    = show v
    string (Uid' u)   = show u
    string (String' s)= show s
    string AABB'      = "AABB"
    string UI'        = "UI"
    string UICont'    = "UIContainer"
    string (UILabel' u)   = "UILabel => " ++ show (uiUid u)
    --string (UIEls' es)= show es
    string UIElCons'  = "UIElementCons"
    string UIElNil'   = "UIElementNil"

instance Type UIFamily Float where
    constructors = [Abstr Float']

instance Type UIFamily Int where
    constructors = [Abstr Int']

instance Type UIFamily (V2 Float) where
    constructors = [Abstr V2']

instance Type UIFamily (V4 Float) where
    constructors = [Abstr V4']

instance Type UIFamily Uid where
    constructors = [Abstr Uid']

instance Type UIFamily String where
    constructors = [Abstr String']

instance Type UIFamily AABB where
    constructors = [Concr AABB']

instance Type UIFamily UI where
    constructors = [Concr UI']

instance Type UIFamily [UIElement] where
    constructors = [Concr UIElCons', Concr UIElNil']

instance Type UIFamily UIElement where
    constructors = [Concr UICont', Concr UILabel']

diffElements :: [UIElement] -> [UIElement] -> EditScript UIFamily [UIElement] [UIElement]
diffElements = diff

showEditScript :: EditScriptL f x y -> String
showEditScript (Ins c d) = "(Ins " ++ string c ++ " " ++ showEditScript d ++ ")"
showEditScript (Del c d) = "(Del " ++ string c ++ " " ++ showEditScript d ++ ")"
showEditScript (Cpy c d) = "(Cpy " ++ string c ++ " " ++ showEditScript d ++ ")"
showEditScript (CpyTree d) = "(CpyTree " ++ showEditScript d ++ ")"
showEditScript End = "End"


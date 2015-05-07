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
--import Control.Monad.State.Strict
--import qualified Data.Set as S
import Data.Set (Set)
--import Data.List (intercalate)
import Debug.Trace

type a $ b = Cons a b
infixr 1 $

data UIFamily :: * -> * -> * where
    Float'  :: Float    -> UIFamily Float Nil
    Int'    :: Int      -> UIFamily Int Nil
    V2'     :: V2 Float -> UIFamily (V2 Float) Nil
    Uid'    :: Uid      -> UIFamily Uid Nil
    String' :: String   -> UIFamily String Nil
    AABB'   :: UIFamily AABB (V2 Float $ V2 Float $ Nil)
    UI'     :: UIFamily UI (Uid $ V2 Float $ V2 Float $ V2 Float $ Float $ Nil)
    UIEls'  :: [UIElement] -> UIFamily [UIElement] Nil
    UICont' :: UIFamily UIElement (UI $ [UIElement] $ Nil)

($$) :: x -> xs -> Cons x xs
a $$ b = CCons a b
infixr 1 $$

pattern a :$ b = CCons a b
infixr 1 :$

instance Family UIFamily where
    decEq (Float' x) (Float' y) | x == y    = Just (Refl, Refl)
                                | otherwise = Nothing
    decEq (Int' x)   (Int' y)   | x == y    = Just (Refl, Refl)
                                | otherwise = Nothing
    decEq (V2'  v)   (V2' w)    | v == w    = Just (Refl, Refl)
                                | otherwise = Nothing
    decEq (Uid' u)   (Uid' d)   | u == d    = Just (Refl, Refl)
                                | otherwise = Nothing
    decEq (String' s)(String' t)| s == t = Just (Refl, Refl)
                                | otherwise = Nothing
    decEq AABB'      AABB'                  = Just (Refl, Refl)
    decEq UI'        UI'                    = Just (Refl, Refl)
    decEq UICont'    UICont'                = Just (Refl, Refl)
    decEq (UIEls' es)(UIEls' es')| es == es' = Just (Refl, Refl)
                                 | otherwise = Nothing
    decEq _             _                   = Nothing

    fields (Float' _)  _          = Just CNil
    fields (Int' _)    _          = Just CNil
    fields (V2' _)     _          = Just CNil
    fields (Uid' _)    _          = Just CNil
    fields (String' _) _          = Just CNil
    fields AABB' (AABB c s)       = Just (c $$ s $$ CNil)
    fields UI' (UI u p sz sc r)   = Just (u $$ p $$ sz $$ sc $$ r $$ CNil)
    fields (UIEls' es) _          = Just CNil
    fields UICont' (UIContainer ui els) = Just (ui $$ els $$ CNil)

    apply (Float' x) CNil = x
    apply (Int' i)   CNil = i
    apply (V2' v)    CNil = v
    apply (Uid' u)   CNil = u
    apply (String' u)CNil = u
    apply AABB' (c :$ s :$ CNil) = AABB c s
    apply UI' (u :$ p :$ sz :$ sc :$ r :$ CNil) = UI u p sz sc r
    apply (UIEls' es) CNil = es
    apply UICont' (ui :$ els :$ CNil) = UIContainer ui els

    string (Float' x) = show x
    string (Int' i)   = show i
    string (V2' v)    = show v
    string (Uid' u)   = show u
    string (String' s)= show s
    string AABB'      = "AABB"
    string UI'        = "UI"
    string UICont'    = "UIContainer"
    string (UIEls' es) = show es

data Edit = Edit { editCurrentUid :: Uid
                 , editDeletions  :: Set Uid
                 }

instance Type UIFamily Float where
    constructors = [Abstr Float']

instance Type UIFamily Int where
    constructors = [Abstr Int']

instance Type UIFamily (V2 Float) where
    constructors = [Abstr V2']

instance Type UIFamily Uid where
    constructors = [Abstr Uid']

instance Type UIFamily String where
    constructors = [Abstr String']

instance Type UIFamily AABB where
    constructors = [Concr AABB']

instance Type UIFamily UI where
    constructors = [Concr UI']

instance Type UIFamily [UIElement] where
    constructors = [Abstr UIEls']

instance Type UIFamily UIElement where
    constructors = [Concr UICont']

runDiff :: EditScript UIFamily UIElement UIElement
runDiff = diff cont1 cont2
    where cont1 = UIContainer u1 []
          cont2 = UIContainer u1 []
          u1    = UI (Uid 1) (V2 0 0) (V2 50 50) (V2 1 1) 0
          u2    = UI (Uid 2) (V2 10 10) (V2 50 50) (V2 1 1) 0

showEdits :: EditScriptL f x y -> String
showEdits (Ins c d) = "(Ins " ++ string c ++ " " ++ showEdits d ++ ")"
showEdits (Del c d) = "(Del " ++ string c ++ " " ++ showEdits d ++ ")"
showEdits (Cpy c d) = "(Cpy " ++ string c ++ " " ++ showEdits d ++ ")"
showEdits (CpyTree d) = "(CpyTree " ++ showEdits d ++ ")"
showEdits End = "End"

--traverseEdits :: EditScriptL f x y -> State Edit ()
--traverseEdits (Ins c d) = undefined
--
--delete :: State Edit ()
--delete = modify $ \(Edit uid set) -> Edit uid $ S.insert uid set

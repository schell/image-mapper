{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.Renderable where

import Types.Internal
import Gelatin.Core.Rendering
import Control.Monad
import Data.Hashable
import Data.IntMap (IntMap)
import Data.List (intercalate)
import Data.Monoid
import qualified Data.IntMap as IM
import GHC.Stack
--------------------------------------------------------------------------------
-- Other Renderables
--------------------------------------------------------------------------------
instance (Renderable a, Hashable a, Show a) => Renderable (Maybe a) where
    cache rz rs (Just a) = cacheIfNeeded rz rs a
    cache _ rs _         = return rs
    nameOf (Just a) = "Just " ++ nameOf a
    nameOf _        = "Nothing"
    renderLayerOf (Just a) = renderLayerOf a
    renderLayerOf _ = []

instance (Renderable a, Hashable a) => Renderable [a] where
    cache rz rs as = foldM (cacheIfNeeded rz) rs as
    nameOf as = "[ " ++ (intercalate ", " $ map nameOf as) ++ " ]"
    renderLayerOf = concatMap renderLayerOf

instance Renderable () where
    cache _ rs ()    = return rs
    nameOf ()        = "()"
    renderLayerOf () = []
--------------------------------------------------------------------------------
-- Cacheing helpers
--------------------------------------------------------------------------------
runLayer :: [(Int, Transform)] -> IntMap Rendering -> Transform -> IO ()
runLayer ts rs t = mapM_ (uncurry go) ts
    where go k t' = maybe (err k) (rend t') $ IM.lookup k rs
          rend t' (Rendering f _) = f $ t <> t'
          err k = errorWithStackTrace $ unwords [ "Fatal error! Could not find"
                                                , "rendering (from a layer)"
                                                , show k
                                                ]

runErr :: (Renderable a, Hashable a, Show a) => a -> IO ()
runErr a = do
    errorWithStackTrace $ unwords [ "Fatal error! Could not find rendering"
                                  , nameOf a
                                  , show $ hash a
                                  , show a
                                  ]

cacheIfNeeded :: (Renderable a, Hashable a)
              => Rez -> IntMap Rendering -> a -> IO (IntMap Rendering)
cacheIfNeeded rz rs a =
    maybe (cache rz rs a) (const $ return rs) $ IM.lookup (hash a) rs

detach :: IntMap Rendering -> Int -> IO (IntMap Rendering)
detach rs k = do
    case IM.lookup k rs of
        Nothing -> errorWithStackTrace $ "Could not find renderer for " ++ show k
        Just (Rendering _ c) -> c
    return $ IM.delete k rs
--------------------------------------------------------------------------------
-- Element
--------------------------------------------------------------------------------
instance Renderable Element where
    cache rz rs (Element a)   = cacheIfNeeded rz rs a
    nameOf (Element a)        = "Element " ++ nameOf a
    renderLayerOf (Element a) = renderLayerOf a

instance Hashable Element where
    hashWithSalt s (Element a) = s `hashWithSalt` "Element" `hashWithSalt` a

instance Eq Element where
    a == b = hash a == hash b

instance Show Element where
    show (Element a) = "Element{ " ++ show a ++ " }"

data Element where
    Element  :: (Show a, Renderable a, Hashable a) => a -> Element
--------------------------------------------------------------------------------
-- Renderable
--------------------------------------------------------------------------------
class Renderable a where
    nameOf        :: a -> String
    cache         :: Rez -> IntMap Rendering -> a -> IO (IntMap Rendering)
    renderLayerOf :: a -> [(Int,Transform)]

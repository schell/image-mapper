{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Renderable where

import Types.Internal
import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Control.Monad
import Data.Hashable
import Data.IntMap (IntMap)
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import qualified Data.IntMap as IM
import GHC.Stack
--------------------------------------------------------------------------------
-- Other Renderables
--------------------------------------------------------------------------------
instance Renderable StandardCursorShape where
    cache (Rez{rezWindow = w}) rs c = do
        cursor <- createStandardCursor c
        let r = Rendering (const $ setCursor w cursor) (destroyCursor cursor)
        return $ IM.insert (hash c) r rs
    renderLayerOf c = [(hash c, Just mempty)]
    nameOf _ = "Cursor"

instance Hashable StandardCursorShape

instance (Renderable a, Hashable a, Show a) => Renderable (Maybe a) where
    cache rz rs (Just a) = cacheIfNeeded rz rs a
    cache _ rs _         = return rs
    nameOf (Just a) = "Just " ++ nameOf a
    nameOf _        = "Nothing"
    renderLayerOf (Just a) = renderLayerOf a
    renderLayerOf _ = []

instance (Renderable a, Hashable a) => Renderable [a] where
    cache = foldM . cacheIfNeeded
    nameOf as = "[ " ++ (intercalate ", " $ map nameOf as) ++ " ]"
    renderLayerOf = concatMap renderLayerOf

instance Renderable () where
    cache _ rs ()    = return rs
    nameOf ()        = "()"
    renderLayerOf () = []
--------------------------------------------------------------------------------
-- Cacheing helpers
--------------------------------------------------------------------------------
runLayer :: Layer -> IntMap Rendering -> Transform -> IO ()
runLayer ts rs t = mapM_ (uncurry go) ts
    where go k (Just t') = maybe (err k) (rend t') $ IM.lookup k rs
          go _ _ = return ()
          rend t' (Rendering f _) = f $ t <> t'
          err k = errorWithStackTrace $ unwords [ "Fatal error! Could not find"
                                                , "rendering (from a layer)"
                                                , show k
                                                ]

runLayerHidden :: Layer -> IntMap Rendering -> Transform -> IO ()
runLayerHidden ts = runLayer (catMaybes $ map f ts)
    where f (i, Nothing) = Just (i, Just mempty)
          f _ = Nothing

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
    cache         :: Rez -> IntMap Rendering -> a -> IO (IntMap Rendering)
    nameOf        :: a   -> String
    renderLayerOf :: a   -> Layer

type Layer = [(Int, Maybe Transform)]

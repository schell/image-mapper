{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Renderable where

import Types.Internal
import Gelatin.Core.Rendering
import Control.Monad
import Data.Hashable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import GHC.Stack
--------------------------------------------------------------------------------
-- Other Renderables
--------------------------------------------------------------------------------
instance (Renderable a, Hashable a, Show a) => Renderable (Maybe a) where
    cache rz rs (Just a) = cacheByProxying (hash $ Just a) rz rs a
    cache rz rs a        = cacheByProxying (hash a) rz rs ()
    nameOf (Just a) = "Just " ++ nameOf a
    nameOf _        = "Nothing"
    transformOf (Just a) = transformOf a
    transformOf _        = mempty
    children (Just a) = children a
    children _        = []
    hashes (Just a) = hash (Just a) : hashes a
    hashes a        = [hash a]

instance Renderable () where
    cache _ rs () = return $ IM.insert (hash ()) mempty rs
    nameOf _      = "()"
    transformOf _ = mempty
    children _    = []
    hashes _      = [hash ()]
--------------------------------------------------------------------------------
-- Cacheing helpers
--------------------------------------------------------------------------------
runRendering :: (Hashable a, Renderable a, Show a)
             => a -> Transform -> IntMap Rendering -> IO ()
runRendering a t = maybe (runErr a) (\(Rendering f _) -> f t) . IM.lookup (hash a)

runCleaning :: (Hashable a, Renderable a, Show a) => a -> IntMap Rendering -> IO ()
runCleaning a = maybe (runErr a) (\(Rendering _ c) -> c) . IM.lookup (hash a)

runErr :: (Renderable a, Hashable a, Show a) => a -> IO ()
runErr a = do
    errorWithStackTrace $ unwords [ "Fatal error! Could not find rendering"
                                  , nameOf a
                                  , show $ hash a
                                  , show a
                                  ]

cacheRenderings :: (Renderable a, Hashable a)
                    => Rez -> IntMap Rendering -> a -> IO (IntMap Rendering)
cacheRenderings rz rs a =
    maybe (cache rz rs a) (const $ return rs) $ IM.lookup (hash a) rs

cacheByProxying :: (Renderable a, Hashable a, Show a)
                => Int -> Rez -> IntMap Rendering -> a -> IO (IntMap Rendering)
cacheByProxying k rz rs a = do
    rs' <- cacheRenderings rz rs a
    let r   = Rendering f c
        c   = return ()
        f t = runRendering a t rs'
    return $ IM.insert k r rs'

cacheChildren :: (Renderable a, Hashable a)
             => Rez -> IntMap Rendering -> a -> IO (IntMap Rendering)
cacheChildren rz rs m = do
    let f rs' a = cacheRenderings rz rs' a
    rs' <- foldM f rs $ children m
    return $ IM.insert (hash m) mempty rs'

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
    cache rz rs e@(Element a) = cacheByProxying (hash e) rz rs a
    nameOf (Element a)      = "Element " ++ nameOf a
    transformOf (Element a) = transformOf a
    children (Element a)    = children a
    hashes (Element a)      = hash (Element a) : hashes a

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
    cache       :: Rez -> IntMap Rendering -> a -> IO (IntMap Rendering)
    nameOf      :: a -> String
    transformOf :: a -> Transform
    children    :: a -> [Element]
    hashes      :: a -> [Int]

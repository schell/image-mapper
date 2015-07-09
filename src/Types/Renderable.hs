{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Renderable where

import Types.Internal
import Gelatin.Core.Render
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import Data.Hashable
import qualified Data.IntMap as IM
import GHC.Stack
--------------------------------------------------------------------------------
-- Other Renderables
--------------------------------------------------------------------------------
instance (Renderable a, Hashable a, Show a) => Renderable [a] where
    render es = do rs <- mapM getRenderer es
                   let f t = mapM_ (\(Renderer r _) -> r t) rs
                   return $ Just $ Renderer f $ return ()
    nameOf _ = "List"
    renderingHashes a = hash a : map hash a
    transformOf _ = mempty

instance (Renderable a, Hashable a, Show a) => Renderable (Maybe a) where
    render (Just a) = do Renderer f _ <- getRenderer a
                         return $ Just $ Renderer f $ return ()
    render _        = return Nothing
    nameOf _ = "Maybe"
    transformOf (Just a) = transformOf a
    transformOf _        = mempty
    renderingHashes (Just a) = hash (Just a) : renderingHashes a
    renderingHashes _ = []

instance Renderable () where
    render _ = return mempty
    nameOf _ = "Unit"
    renderingHashes _ = [hash ()]
    transformOf _ = mempty
--------------------------------------------------------------------------------
-- Rendering ops all elements may need
--------------------------------------------------------------------------------
getRenderer :: (MakesScene r, Renderable a, Hashable a, Show a)
            => a -> Eff r Renderer
getRenderer a = do
    let h = hash a
    mAR <- lookupAttached h
    case mAR of
        Nothing -> newRenderer h a
        Just r  -> return r
--------------------------------------------------------------------------------
-- Handling Renderers as Resources
--------------------------------------------------------------------------------
-- | Create a new renderer for an element and attaches it with the given
-- UID. If the renderer cannot be created return a no-op and attach
-- nothing.
newRenderer :: (Renderable a, MakesScene r, Hashable a)
            => Int -> a -> Eff r Renderer
newRenderer i el = do
    mr <- render el
    r <- case mr of
        Just r  -> attach i (nameOf el) r >> return r
        Nothing -> return mempty
    return r

attach :: (ModifiesRenderers r, DoesIO r)
       => Int -> String -> Renderer -> Eff r ()
attach i s a' = do
    lift $ putStrLn $ "Attaching " ++ show s ++ " (" ++ show i ++ ")"
    Attached rs <- get
    case IM.lookup i rs of
        Nothing -> do put $ Attached $ IM.insert i a' rs
                      modify $ \(Named ns) -> Named $ IM.insert i s ns
        Just _  -> do mname <- lookupName i
                      lift $ putStrLn $ unwords [ "Fatal attachment collision"
                                                , "while attempting to insert"
                                                , s ++ "."
                                                , "Found renderer"
                                                , show mname ++ "."
                                                ]
                      errorWithStackTrace "Aborting"

detach :: (ModifiesRenderers r, DoesIO r) => Int -> Eff r ()
detach i = do
    mr <- lookupAttached i
    mn <- lookupName i
    case mr of
        Nothing -> do lift $ putStrLn $ unwords [ "Fatal detachment error."
                                                , "Could not find renderer"
                                                , show i ++ "."
                                                ]
                      errorWithStackTrace "Aborting"
        Just r -> do lift $ putStrLn $ unwords [ "Detaching"
                                               , show mn
                                               , "( " ++ show i ++ " )"
                                               ]
                     modify $ \(Attached rs) -> Attached $ IM.delete i rs
                     modify $ \(Named rs) -> Named $ IM.delete i rs
                     (\(Renderer _ c) -> lift c) r


lookupAttached :: (Member (State AttachedRenderers) r)
               => Int -> Eff r (Maybe Renderer)
lookupAttached i = (IM.lookup i . attached) <$> get

lookupName :: (Member (State NamedRenderers) r) => Int -> Eff r (Maybe String)
lookupName i = (IM.lookup i . named) <$> get
--------------------------------------------------------------------------------
-- Element
--------------------------------------------------------------------------------
instance Renderable Element where
    render (Element a)          = do Renderer f _ <- getRenderer a
                                     return $ Just $ Renderer f $ return ()
    nameOf (Element _)          = "Element"
    transformOf (Element a)     = transformOf a
    renderingHashes (Element a) = hash (Element a) : renderingHashes a

instance Hashable Element where
    hashWithSalt s (Element a) = s `hashWithSalt` "Element" `hashWithSalt` a

instance Eq Element where
    (Element a) == (Element b) = hash a == hash b

instance Show Element where
    show (Element a) = "Element{ " ++ show a ++ " }"

data Element where
    Element  :: (Show a, Renderable a, Hashable a) => a -> Element
--------------------------------------------------------------------------------
-- Renderable
--------------------------------------------------------------------------------
class Renderable a where
    render :: ( Member (Reader Rez) r
              , Member (State AttachedRenderers) r
              , Member (State NamedRenderers) r
              , SetMember Lift (Lift IO) r
              )
           => a -> Eff r (Maybe Renderer)

    nameOf :: a -> String
    transformOf :: a -> Transform

    renderingHashes :: a -> [Int]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network
import Scene
import Prelude hiding (sequence_)
import System.Remote.Monitoring
import Gelatin.Core.Render
--import Entity
import Control.Concurrent.Async
import Graphics.GL.Core33
import Control.Varying
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Arrow
import Data.Bits
import Data.Typeable
import Data.IORef
import Data.Monoid
--import Data.Maybe
--import Data.Foldable (sequence_)
import Data.IntMap (IntMap)
--import Data.Map (Map)
import Data.Time.Clock
import System.Exit
import qualified Data.IntMap as IM
--import qualified Data.Map as M


newtype Rendering = Rendering { unRendering :: Renderer } deriving (Typeable)
--type Renderings = IntMap Rendering
--type Transforms = IntMap Transform
--type ParentEntities = IntMap UniqueId
--type Names = Map String UniqueId


--transforms :: (Mutates Transforms r,
--               Mutates ParentEntities r)
--           => Eff r Transforms
--transforms = do
--    ts <- get
--    parents <- get
--    let displayMap = fmap findParents parents
--        findParents p = allParents parents p ++ [p]
--        parentTfrms = fmap (foldr mappend mempty . catMaybes . fmap findTfrms) displayMap
--        findTfrms = flip IM.lookup ts . unId
--    return $ IM.unionWith mappend ts parentTfrms
--
---- | Lists a branch of all parents, root first.
--allParents :: ParentEntities -> UniqueId -> [UniqueId]
--allParents parents uid =
--    -- Precaution so we don't recurse if a parent contains itself
--    takeWhile (/= uid) allParents'
--    where allParents' = case IM.lookup (unId uid) parents of
--                            Nothing -> []
--                            Just uid' -> allParents parents uid' ++ [uid']

main :: IO ()
main = do
    putStrLn "aoeusnth"
    void $ forkServer "localhost" 8000
    win <- initWindow 800 600 "Syndeca Mapper"

    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource

    --- Load an image texture
    --Right img  <- readImage "/Users/schell/Desktop/KDC_desktop.jpg"
    --let w = fromIntegral $ dynamicMap imageWidth img
    --    h = fromIntegral $ dynamicMap imageHeight img
    --    texTfrm = translate 110 110 mempty
    --tex <- loadTexture img
    --texR <- textureRenderer win grs tex GL_TRIANGLES
    --                        [V2 0 0, V2 w 0, V2 w h
    --                        ,V2 0 0, V2 0 h, V2 w h]
    --                        [V2 0 0, V2 1 0, V2 1 1
    --                        ,V2 0 0, V2 0 1, V2 1 1]

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    ref <- newIORef []
    let input i = modifyIORef ref (++ [i])

    setCharCallback win $ Just $ \_ c -> input $
        CharEvent c

    setWindowSizeCallback win $ Just $ \_ w' h' -> input $
        WindowSizeEvent w' h'

    setKeyCallback win $ Just $ \_ k i ks modi -> input $
        KeyEvent k i ks modi

    setMouseButtonCallback win $ Just $ \_ mb mbs modi -> input $
        MouseButtonEvent mb mbs modi

    setCursorPosCallback win $ Just $ \_ x y -> input $
        CursorMoveEvent x y

    setCursorEnterCallback win $ Just $ \_ cs -> input $
        CursorEnterEvent cs

    setScrollCallback win $ Just $ \_ x y -> input $
        ScrollEvent x y

    setDropCallback win $ Just $ \_ fs -> do
        putStrLn $ "Got files:\n" ++ unlines fs
        input $ FileDropEvent fs

    afc <- compileFontCache
    let rez = Rez grs brs win afc

    runLift $ flip runReader rez
            $ flip runFresh (Uid 0)
            -- $ evalState (IM.empty :: IntMap
            $ step ref network

step :: (Member (Reader Rez) r,
        Member (Fresh Uid) r,
        SetMember Lift (Lift IO) r)
     => IORef [InputEvent] -> Var (Eff r) InputEvent Scene -> Eff r ()
step ref net = do
    win <- rezWindow <$> ask
    es <- lift $ readIORef ref
    lift $ writeIORef ref []
    (scene, net') <- stepMany NoInputEvent es net
    lift $ stepScene win scene
    step ref net'

stepMany :: Monad m => a -> [a] -> Var m a b -> m (b, Var m a b)
stepMany a []  y = runVar y a
stepMany a' (a:as) y = execVar y a >>= stepMany a' as



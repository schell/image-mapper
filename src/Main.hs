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
import UI
import Scene
import Prelude hiding (sequence_)
import System.Remote.Monitoring
import Gelatin.Core.Render
import Graphics.GL.Core33
import Control.Varying
import Control.Monad
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Data.IORef
import Data.Time.Clock

main :: IO ()
main = do
    putStrLn "aoeusnth"
    void $ forkServer "localhost" 8000
    win <- initWindow 800 600 "Syndeca Mapper"

    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource

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
    t   <- getCurrentTime
    let rez   = Rez grs brs win afc

    runLift $ flip runReader rez
            $ flip runFresh (Uid 0)
            $ evalState (Attached mempty)
            $ evalState t
            $ evalState (Delta 0)
            $ step ref uinetwork

step :: (MakesScene r, TimeDelta r, Member (State UTCTime) r)
     => IORef [InputEvent] -> Var (Eff r) InputEvent [(Element, Transform)] -> Eff r ()
step ref net = do
    -- Update input events.
    es <- lift $ readIORef ref
    lift $ writeIORef ref []

    -- Update time delta.
    t  <- get
    t' <- lift $ getCurrentTime
    put $ Delta $ realToFrac $ t' `diffUTCTime` t

    (els, net') <- stepMany es net

    stepScene els
    step ref net'

stepMany :: Monad m => [InputEvent] -> Var m InputEvent b -> m (b, Var m InputEvent b)
stepMany (e:[]) y = runVar y e
stepMany (e:es) y = execVar y e >>= stepMany es
stepMany []     y = runVar y NoInputEvent

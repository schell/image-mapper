{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Network
import UI
import Scene
import Prelude hiding (sequence_)
import Linear
import System.Remote.Monitoring
import System.Environment
import System.Exit
import Gelatin.Core.Render
import Gelatin.Core.Color
import Graphics.UI.GLFW
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
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    usingRift <- do args <- getArgs
                    case args of
                        "rift":_ -> return True
                        _        -> return False

    void $ forkServer "localhost" 8000

    if usingRift then startupRift else startup

startup :: IO ()
startup = do
    True <- initGelatin
    w    <- newWindow 800 600 "Syndeca Mapper" Nothing Nothing
    setWindowPos w 925 800
    startupRest w

startupRift :: IO ()
startupRift = do
    putStrLn "Starting up the Rift."
    --1 <- c'ovr_Initialize 0

    True <- initGelatin

    mmons <- getMonitors
    names <- catMaybes <$> case mmons of
                 Just mons -> mapM (\mon -> do mname <- getMonitorName mon
                                               return $ (mon,) <$> mname)
                                   mons
                 Nothing   -> return []

    (mon,name) <- case filter (("Rift" `isPrefixOf`) . snd) names of
                      a:_ -> return a
                      _   -> do putStrLn "Could not find the rift."
                                exitFailure
    newWindow 800 600 name (Just mon) Nothing >>= startupRest

startupRest :: Window -> IO ()
startupRest win = do
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

    cursor <- createStandardCursor StandardCursorShape'Hand
    setCursor win cursor

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
     => IORef [InputEvent] -> Var (Eff r) InputEvent [Element] -> Eff r ()
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

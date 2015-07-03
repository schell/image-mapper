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
import Scene
import Types
import Prelude hiding (sequence_)
import System.Remote.Monitoring
import System.Environment
import System.Exit
import Gelatin.Core.Render
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent
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
import Data.Bits

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

    --cursor <- createStandardCursor StandardCursorShape'Hand
    --setCursor win cursor

    afc <- compileFontCache
    t   <- getCurrentTime
    let rez   = Rez grs brs win afc

    runLift $ flip runReader rez
            $ flip runFresh (Uid 0)
            $ evalState (Attached mempty)
            $ evalState (UsedThisFrame mempty)
            $ evalState t
            $ evalState (Delta 0)
            $ step ref (Element <$> network)

step :: ( MakesScene r
        , StatsRenderers r
        , TimeDelta r
        , Member (State UTCTime) r
        )
     => IORef [InputEvent] -> Var (Eff r) InputEvent Element -> Eff r ()
step ref net = do
    -- Update input events.
    events <- lift $ readIORef ref
    lift $ writeIORef ref []

    -- Update time delta.
    t  <- get
    t' <- lift $ getCurrentTime
    put $ Delta $ realToFrac $ t' `diffUTCTime` t

    -- Step the network
    (container, net') <- stepMany events net

    renderFrame [Element container]
    step ref net'

stepMany :: (Monad m, Monoid a) => [a] -> Var m a b -> m (b, Var m a b)
stepMany (e:[]) y = runVar y e
stepMany (e:es) y = execVar y e >>= stepMany es
stepMany []     y = runVar y mempty

renderFrame :: (MakesScene r, StatsRenderers r) => [Element] -> Eff r ()
renderFrame els = do
    win <- rezWindow <$> ask
    lift $ do
        (fbw,fbh) <- getFramebufferSize win
        glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    mapM_ renderElement els

    lift $ do
        pollEvents
        swapBuffers win
        shouldClose <- windowShouldClose win
        if shouldClose
        then exitSuccess
        else threadDelay 100

    dropUnusedRenderers


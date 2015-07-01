{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
import Control.Concurrent
import Data.IORef
import Data.Time.Clock
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.IORef
import Data.Bits
import Control.Monad.State.Strict


data Sub = Sub { subSource :: String }
data Pub a = Pub { pubThing :: a }

main :: IO ()
main = do
    print "push test"

    True <- initGelatin
    win  <- newWindow 800 600 "Syndeca Mapper" Nothing Nothing

    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    charRef <- newIORef Nothing
    strRef  <- newIORef []
    setCharCallback win $ Just $ \_ c -> modifyIORef charRef (const $ Just c)

    {-

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
    -}

    t   <- getCurrentTime
    let loop = do pollEvents

                  mc <- readIORef charRef
                  ms <- readIORef strRef



                  -- Update input events.
                  --es <- readIORef ref
                  --writeIORef ref []

                  -- Update time delta.
                  --t  <- get
                  --t' <- lift $ getCurrentTime
                  --put $ Delta $ realToFrac $ t' `diffUTCTime` t

                  (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Prelude hiding (sequence_)


main :: IO ()
main = do
    print "push test"

    {-
    True <- initGelatin
    win  <- newWindow 800 600 "Syndeca Mapper" Nothing Nothing

    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    charRef <- newIORef Nothing
    strRef  <- newIORef []
    setCharCallback win $ Just $ \_ c -> modifyIORef charRef (const $ Just c)


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

    -}

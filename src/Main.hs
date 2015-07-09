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
import System
import UI.Picture
import Types.Internal
import Types.Renderable
import Prelude hiding (sequence_, all)
import System.Remote.Monitoring
import System.Environment
import System.Exit
import Gelatin.Core.Render
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent
import Control.Concurrent.Async
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
import qualified Data.IntMap as IM
import qualified Data.IntSet as S

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
    mrs <- loadMaskRenderSource

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
    let rez   = Rez grs brs mrs win afc

    runLift $ flip runReader rez
            $ flip runFresh (Uid 0)
            $ evalState (Attached mempty)
            $ evalState (Named mempty)
            -- $ evalState (Requests mempty)
            $ evalState t
            $ step ref network

step :: ( MakesScene r
        --, StatsRenderers r
        , Member (State UTCTime) r
        --, Member (State Requests) r
        )
     => IORef [InputEvent] -> Var (Eff r) InputEvent Network -> Eff r ()
step ref net = do
    -- Update input events.
    events <- lift $ readIORef ref
    lift $ writeIORef ref []

    -- Update running requests.
    --results <- (map $ uncurry RequestEvent) <$> updateRequests

    -- Update time delta.
    t  <- get
    t' <- lift $ getCurrentTime
    put t'
    let dt = realToFrac $ t' `diffUTCTime` t
        events' = {-results ++ -}events ++ [TimeDeltaEvent dt]

    -- Step the network
    (el, net') <- stepMany events' net

    renderFrame [Element el]

    --mapM_ insertRequest $ networkRequests el

    step ref net'

updateRequests :: ( Member (State Requests) r
                  , SetMember Lift (Lift IO) r
                  ) => Eff r [(Uid, RequestResult)]
updateRequests = do
    Requests reqs <- get
    polls <- lift $ sequence $ poll <$> reqs
    let f (Just (Right a)) = Just a
        f _ = Nothing
    return $ map (\(k,a) -> (Uid k, a)) $ IM.toList $ IM.mapMaybe f polls

insertRequest :: ( Member (State Requests) r
              , SetMember Lift (Lift IO) r
              )
           => Request -> Eff r ()
insertRequest (ReqPicInfo (Uid k) fp) = do
    a <- lift $ async $ do mp <- loadPicture fp
                           case mp of
                               Nothing -> fail "Could not fullfill request for pic info."
                               Just (Pic _ _ w h)  -> return $ ResultPicInfo $ PicInfo w h
    modify $ \(Requests m) -> Requests $ IM.insert k a m

stepMany :: (Monad m, Monoid a) => [a] -> Var m a b -> m (b, Var m a b)
stepMany (e:[]) y = runVar y e
stepMany (e:es) y = execVar y e >>= stepMany es
stepMany []     y = runVar y mempty

renderFrame :: (MakesScene r) => [Element] -> Eff r ()
renderFrame els = do
    win <- rezWindow <$> ask
    lift $ do
        (fbw,fbh) <- getFramebufferSize win
        glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    rs <- mapM getRenderer els
    mapM_ (\(Renderer r _, el) -> lift $ r $ transformOf el) $ zip rs els

    lift $ do
        pollEvents
        swapBuffers win
        shouldClose <- windowShouldClose win
        if shouldClose
        then exitSuccess
        else threadDelay 100

    -- Detach any renderers that were not used this frame.
    Attached all <- get
    let hs = S.fromList $ concatMap renderingHashes els
        ks = IM.keysSet all
        old = S.difference ks hs
    when (S.size old > 1) $ do
        lift $ putStrLn ""
        mapM_ detach $ S.toList old
        lift $ putStrLn ""

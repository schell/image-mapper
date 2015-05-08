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

--------------------------------------------------------------------------------
-- Stuff I might need later
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

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
    let rez   = Rez grs brs win afc

    runLift $ flip runReader rez
            $ flip runFresh (Uid 1)
            $ evalState ([] :: [UIElement])
            $ evalState (mempty :: Scene)
            $ step ref uinetwork

step :: (Member (Reader Rez) r,
         Member (Fresh Uid) r,
         Member (State [UIElement]) r,
         Member (State Scene) r,
         SetMember Lift (Lift IO) r)
     => IORef [InputEvent] -> Var (Eff r) InputEvent [UIElement] -> Eff r ()
step ref net = do
    win <- rezWindow <$> ask
    es <- lift $ readIORef ref
    lift $ writeIORef ref []

    (els', net') <- stepMany es net
    els   <- get
    scene <- get

    let edit  = diffElements els els'
    scene' <- lift $ editScene edit scene
    --put root'
    --put scene'

    lift $ stepScene win scene'
    step ref net'

stepMany :: Monad m => [InputEvent] -> Var m InputEvent b -> m (b, Var m InputEvent b)
stepMany (e:[]) y = runVar y e
stepMany (e:es) y = execVar y e >>= stepMany es
stepMany []     y = runVar y NoInputEvent



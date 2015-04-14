{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Example
import Prelude hiding (sequence_)
import Gelatin.Core.Render
import Entity
import Graphics.GL.Core33
import Reflex
import Reflex.Host.Class
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Applicative
import Control.Monad hiding (sequence_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.Bits
import Data.Typeable
import Data.IORef
import Data.Monoid
import Data.Maybe
import Data.Foldable (sequence_)
import Data.IntMap (IntMap)
import Data.Map (Map)
import System.Exit
import qualified Data.IntMap as IM
import qualified Data.Map as M


newtype Rendering = Rendering { unRendering :: Renderer } deriving (Typeable)
--type Renderings = IntMap Rendering
--type Transforms = IntMap Transform
--type ParentEntities = IntMap UniqueId
--type Names = Map String UniqueId
data InputEvent = NoInputEvent
                | CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                -- ^ Key, scancode, pressed/released, mods
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                | FileDropEvent [String]
                deriving (Show, Eq, Ord)

data Scene = Scene [Rendering] [Transform]

type CursorApp t m = (Reflex t, MonadHold t m, MonadFix m)
                   => Event t InputEvent
                   -> m (Behavior t Scene)

host :: Window
     -> (forall t m. CursorApp t m)
     -> IO ()
host w myGuest = runSpiderHost $ do

    (e, eTriggerRef) <- newEventWithTriggerRef
    eHandle <- subscribeEvent e

    cbRef <- liftIO $ newIORef []

    let input i = modifyIORef cbRef (++ [i])

    liftIO $ do
        setCharCallback w $ Just $ \_ c -> input $
            CharEvent c

        setWindowSizeCallback w $ Just $ \_ w' h' -> input $
            WindowSizeEvent w' h'

        setKeyCallback w $ Just $ \_ k i ks modi -> input $
            KeyEvent k i ks modi

        setMouseButtonCallback w $ Just $ \_ mb mbs modi -> input $
            MouseButtonEvent mb mbs modi

        setCursorPosCallback w $ Just $ \_ x y -> input $
            CursorMoveEvent x y

        setCursorEnterCallback w $ Just $ \_ cs -> input $
            CursorEnterEvent cs

        setScrollCallback w $ Just $ \_ x y -> input $
            ScrollEvent x y

        setDropCallback w $ Just $ \_ fs -> do
            putStrLn $ "Got files:\n" ++ unlines fs
            input $ FileDropEvent fs

    b <- runHostFrame $ myGuest e
    forever $ do
        mETrigger <- liftIO $ readIORef eTriggerRef
        case mETrigger of
            Nothing -> return ()
            Just t  -> do
                events <- liftIO $ readIORef cbRef
                let events' = map (t :=>) events
                fireEventsAndRead events' $ void $ readEvent eHandle
                liftIO $ writeIORef cbRef []
        scene <- runHostFrame $ sample b
        liftIO $ renderScene w scene

mouseFollow :: Rendering -> CursorApp t m
mouseFollow r e = do
    let rs = constDyn [r]
        takeCursor t (CursorMoveEvent x y) = translate (realToFrac x) (realToFrac y) t
        takeCursor t _ = t

    t <- foldDyn takeCursor mempty

    return $ Scene <$> rs <*> [t]

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
    win <- initWindow 800 600 "Syndeca Mapper"

    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource

    --- Load an image texture
    Right img  <- readImage "/Users/schell/Desktop/KDC_desktop.jpg"
    let w = fromIntegral $ dynamicMap imageWidth img
        h = fromIntegral $ dynamicMap imageHeight img
        texTfrm = translate 110 110 mempty
    tex <- loadTexture img
    texR <- textureRenderer win grs tex GL_TRIANGLES
                            [V2 0 0, V2 w 0, V2 w h
                            ,V2 0 0, V2 0 h, V2 w h]
                            [V2 0 0, V2 1 0, V2 1 1
                            ,V2 0 0, V2 0 1, V2 1 1]

    -- Load some lines
    let lineTfrm = translate 10 10 mempty
    lineR <- colorRenderer win grs GL_LINES
                           [V2 0 0, V2 100 0, V2 100 0, V2 0 100, V2 0 100, V2 0 0]
                           (cycle [V4 1 1 0 1, V4 1 0 1 1])

    -- Load some beziers
    let bezTfrm = translate 110 10 mempty
    bezR <- colorBezRenderer win brs
                             [Bezier GT (V2 0 0) (V2 100 0) (V2 100 100)]
                             [Triangle (V4 1 1 0 1) (V4 1 0 1 1) (V4 1 1 0 1)]

    -- Load a font renderer
    dpi <- calculateDpi
    afc <- compileFontCache
    void $ wait afc
    let textTfrm = translate 0 (110 + 32) mempty
    Just (textR, tex2R) <- withFont afc (FontDescriptor "Arial" $ FontStyle False False) $ \font -> do
        textR <- colorFontRenderer win grs brs dpi
                                   (FontString font 32 "aoeusnth")
                                   (\(V2 x _) ->
                                       lerp (x/100) (V4 1 1 0 1) (V4 1 0 1 1))

        tex2 <- toTexture (100, 100) $ do (rRender lineR) lineTfrm
                                          (rRender bezR) bezTfrm
                                          (rRender textR) textTfrm

        texDrawing2 <- textureRenderer win grs tex2 GL_TRIANGLES
                                       [V2 0 0, V2 w 0, V2 w h
                                       ,V2 0 0, V2 0 h, V2 w h]
                                       [V2 0 0, V2 1 0, V2 1 1
                                       ,V2 0 0, V2 0 1, V2 1 1]
        return (textR, texDrawing2)

    let tfrms = [texTfrm, lineTfrm, bezTfrm, textTfrm, translate 100 100 mempty]
        rndrs = [texR, lineR, bezR, textR, tex2R]

    --runLift $ evalState (IM.empty :: Transforms)
    --        $ evalState (IM.empty :: ParentEntities)
    --        $ evalState (IM.empty :: Renderings)
    --        $ evalState (M.empty :: Names)
    --        $ flip runFresh (UniqueId 0)
    --        $ do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    --    entity ## Rendering texR
    --           .# texTfrm

    --    entity ## Rendering lineR
    --           .# lineTfrm

    --    entity ## Rendering bezR
    --           .# bezTfrm

    --    entity ## Rendering textR
    --           .# textTfrm

    --    entity ## Rendering tex2R
    --           .# translate 100 100 mempty


    --runSpiderHost

    host undefined

renderScene win (Scene rndrs tfrms) = do
    (fbw,fbh) <- getFramebufferSize win
    glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    --rs <- (fmap unRendering) <$> get
    --ts <- transforms
    --lift $ sequence_ $ IM.intersectionWith rRender rs ts
    zipWithM_ rRender rndrs tfrms

    pollEvents
    swapBuffers win
    shouldClose <- windowShouldClose win
    if shouldClose
    then exitSuccess
    else threadDelay 100

--doEvent (CursorMoveEvent x y) = do
--    mbutton <- getEntityBy ("button" :: String)
--    case mbutton of
--        Nothing -> return ()
--        Just b  -> modify $ IM.adjust (\(Transform _ s r) ->
--                          Transform (realToFrac <$> V2 x y) s r) (unId b)

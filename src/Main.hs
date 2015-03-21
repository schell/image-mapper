{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (sequence_)
import Render
import Entity
import Linear
import Graphics.Text.TrueType
import Control.Applicative
import Control.Monad hiding (sequence_)
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.State.Strict
import Data.IORef
import Data.Monoid
import Data.Maybe
import Data.Foldable (sequence_)
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.IntMap as IM
import qualified Data.Map as M

type Colors = IntMap Color
type Transforms = IntMap Transform
type ParentEntities = IntMap UniqueId
type Displays = IntMap Display
type Names = Map String UniqueId
type Mutates a = Member (State a)
type MakesIds = Member (Fresh UniqueId)
type DoesIO = SetMember Lift (Lift IO)
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

-- | Inject some input into a WindowRef.
input :: IORef [InputEvent] -> InputEvent -> IO ()
input ref e = modifyIORef ref (++ [e])

setInputCallbacks :: IORef [InputEvent] -> Window -> IO ()
setInputCallbacks ref win = do
    setCharCallback win $ Just $ \_ c ->
        input ref $ CharEvent c

    setWindowSizeCallback win $ Just $ \_ w' h' -> do
        input ref $ WindowSizeEvent w' h'

    setKeyCallback win $ Just $ \_ k i ks modi ->
        input ref $ KeyEvent k i ks modi

    setMouseButtonCallback win $ Just $ \_ mb mbs modi ->
        input ref $ MouseButtonEvent mb mbs modi

    setCursorPosCallback win $ Just $ \_ x y ->
        input ref $ CursorMoveEvent x y

    setCursorEnterCallback win $ Just $ \_ cs ->
        input ref $ CursorEnterEvent cs

    setScrollCallback win $ Just $ \_ x y ->
        input ref $ ScrollEvent x y

    setDropCallback win $ Just $ \_ fs -> do
        putStrLn $ "Got files:\n" ++ unlines fs
        input ref $ FileDropEvent fs


transforms :: (Mutates Transforms r,
               Mutates ParentEntities r,
               Mutates Displays r)
           => Eff r Transforms
transforms = do
    ts <- get
    parents <- get
    let displayMap = fmap findParents parents
        findParents p = allParents parents p ++ [p]
        parentTfrms = fmap (foldr mappend mempty . catMaybes . fmap findTfrms) displayMap
        findTfrms = flip IM.lookup ts . unId
    return $ IM.unionWith mappend ts parentTfrms

-- | Lists a branch of all parents, root first.
allParents :: ParentEntities -> UniqueId -> [UniqueId]
allParents parents uid =
    -- Precaution so we don't recurse if a parent contains itself
    takeWhile (/= uid) allParents'
    where allParents' = case IM.lookup (unId uid) parents of
                            Nothing -> []
                            Just uid' -> allParents parents uid' ++ [uid']

main :: IO ()
main = do
    putStrLn "aoeusnth"
    rsrcs <- initResources 800 600 "Syndeca Mapper"
    let win = rsrcWindow rsrcs

    ref   <- newIORef []
    setInputCallbacks ref win

    runLift $ evalState (IM.empty :: Transforms)
            $ evalState (IM.empty :: ParentEntities)
            $ evalState (IM.empty :: Displays)
            $ evalState (IM.empty :: Colors)
            $ evalState (M.empty :: Names)
            $ evalState rsrcs
            $ flip runFresh (UniqueId 0)
            $ app ref

app :: (Mutates ParentEntities r,
        Mutates Transforms r,
        Mutates Colors r,
        Mutates Displays r,
        Mutates Resources r,
        Mutates Names r,
        MakesIds r,
        DoesIO r)
    => IORef [InputEvent] -> Eff r ()
app ref = do
    enableBlending

    entity ## Transform (V2 0 60) (V2 1 1) 0
           ## (SolidColor $ V4 1 0 0 1)
           .# DisplayText' ubuntuMono{_descriptorStyle = FontStyle True False} 128 "{}/~Test test test"

    button <- entity ## Transform (V2 100 100) (V2 1 1) 0
                     -- ## SolidColor (V4 1 0 0 1)
                     ## DisplayPoly [V2 0 0, V2 100 0, V2 100 40, V2 0 40]
                     ## (TextureColor (LocalImage "/Users/schell/Desktop/IMG_0066.PNG") [V2 1 1, V2 1 0, V2 0 0, V2 0 1])
                     `named` "button"

    entity `inside` button
           ## Transform (V2 10 32) (V2 1 1) 0
           ## (SolidColor $ V4 1 1 1 1)
           ## DisplayText ubuntuMono 32 "Start"
           .# button

    forever $ do
        -- Get and process our events this frame.
        events <- lift $ do events <- readIORef ref
                            writeIORef ref []
                            return events
        forM_ events doEvent

        -- Render!
        withNewFrame $ do
            drawClear
            ts <- transforms
            ds <- get
            cs <- get
            sequence_ $ intersectionWithKey3 draw cs ds ts

doEvent :: (Mutates ParentEntities r,
            Mutates Transforms r,
            Mutates Colors r,
            Mutates Displays r,
            Mutates Resources r,
            Mutates Names r,
            MakesIds r,
            DoesIO r)
        => InputEvent -> Eff r ()
doEvent (CursorMoveEvent x y) = do
    mbutton <- getEntityBy "button"
    case mbutton of
        Nothing -> return ()
        Just b  -> modify $ IM.adjust (\(Transform _ s r) ->
                          Transform (realToFrac <$> V2 x y) s r) (unId b)
doEvent _ = return ()

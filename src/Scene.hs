{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scene where

import Prelude hiding (sequence_)
import UI
import Data.Generic.Diff
import Gelatin.Core.Render
import Control.Concurrent.Async
import Graphics.GL.Core33
import Control.Monad
import Control.Concurrent
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Data.Bits
import Data.Typeable
import Data.IntMap (IntMap)
import System.Exit
import qualified Data.IntMap as IM

data Scene = Scene { sceneRenderers  :: IntMap Renderer
                   , sceneTransforms :: IntMap Transform
                   }

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezWindow    :: Window
               , rezFontCache :: Async FontCache
               } deriving (Typeable)

type MakesScene r = (Member (Reader Rez) r, SetMember Lift (Lift IO) r)

instance Monoid Scene where
    mempty = Scene mempty mempty
    mappend (Scene rs ts) (Scene rs' ts') = Scene (rs `mappend` rs')
                                                  (ts `mappend` ts')

newtype Indent = Indent Int deriving (Typeable, Eq, Ord)

space :: Int -> String
space i = concat $ replicate i "  "


spacePrint :: (SetMember Lift (Lift IO) r, Member (State [Int]) r) => String -> Eff r ()
spacePrint s = do
    (is :: [Int]) <- get
    lift $ putStrLn $ space (sum is) ++ s

editScene :: [UIElement] -> [UIElement] -> Scene -> IO Scene
editScene els els' (Scene rs ts) = do
    putStrLn "\n"
    let edit = diffElements els els'
    runLift $ evalState rs
            $ evalState ts
            $ evalState ([] :: [Int])
            $ evalState els
            $ performEdits edit

performEdits :: (SetMember Lift (Lift IO) r,
                 Member (State (IntMap Renderer)) r,
                 Member (State (IntMap Transform)) r,
                 Member (State [Int]) r)
             => EditScriptL UIFamily x y -> Eff r Scene
performEdits (Ins UIElCons' es) = do
    pop
    spacePrint ", (insert)"
    performEdits es

performEdits (Ins c es) = do
    pop
    spacePrint $ "Insert:  " ++ string c
    performEdits es

performEdits (Del c es) = do
    pop
    spacePrint $ "Delete:  " ++ string c
    performEdits es
--------------------------------------------------------------------------------
-- Copying
--------------------------------------------------------------------------------
performEdits (Cpy UIElCons' es) = do
    spacePrint "Copy ["
    push 1
    performEdits es

performEdits (Cpy UIElNil' es) = do
    pop
    spacePrint "Copy ]"
    performEdits es

performEdits (Cpy UILabel' es) = do
    spacePrint "Copy UILabel"
    push 4
    performEdits es

performEdits (Cpy UICont' es) = do
    spacePrint "Copy UIContainer"
    push 2
    performEdits es

performEdits (Cpy UI' es) = do
    spacePrint "Copy UI"
    push 5
    performEdits es

performEdits (Cpy c es) = do
    pop
    spacePrint $ "Copy:    " ++ string c
    performEdits es

performEdits (CpyTree es) = do
    pop
    spacePrint "CopyTree"
    performEdits es

performEdits End = do
    rs <- get
    ts <- get
    return $ Scene rs ts

push :: (Member (State [Int]) r) => Int -> Eff r ()
push i = modify (i:)

pop :: (Member (State [Int]) r) => Eff r ()
pop = do
   (cs :: [Int]) <- get
   case cs of
       []     -> return ()
       (i:is) -> case i - 1 of
                     0 -> put is
                     x -> put $ x:is

decIndent :: (Member (State Indent) r) => Eff r ()
decIndent = modify $ \(Indent i) -> Indent (i - 1)

stepScene :: Window -> Scene -> IO ()
stepScene win scene = do
    (fbw,fbh) <- getFramebufferSize win
    glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    renderScene scene

    pollEvents
    swapBuffers win
    shouldClose <- windowShouldClose win
    if shouldClose
    then exitSuccess
    else threadDelay 100

renderScene :: Scene -> IO ()
renderScene (Scene rs ts) = sequence_ $ IM.intersectionWith rRender rs ts

mkSceneFromUI :: UIElement -> IO Scene
mkSceneFromUI = undefined

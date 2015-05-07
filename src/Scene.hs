{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Scene where

import Prelude hiding (sequence_)
import System.Remote.Monitoring
import Gelatin.Core.Render
import Control.Concurrent.Async
import Graphics.GL.Core33
import Control.Varying
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Arrow
import Data.Bits
import Data.Typeable
import Data.IORef
import Data.Monoid
--import Data.Maybe
--import Data.Foldable (sequence_)
import Data.IntMap (IntMap)
--import Data.Map (Map)
import Data.Time.Clock
import System.Exit
import qualified Data.IntMap as IM
--import qualified Data.Map as M

data Scene = Scene Transform Renderer
           | SceneContainer Transform [Scene]

instance Monoid Scene where
    mempty = Scene mempty mempty
    mappend a b = SceneContainer mempty [a,b]

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezWindow    :: Window
               , rezFontCache :: Async FontCache
               } deriving (Typeable)

type MakesScene r = (Member (Reader Rez) r, SetMember Lift (Lift IO) r)

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

renderScene (SceneContainer t ss) = forM_ ss $ \(Scene t' r) -> rRender r (t <> t')
renderScene (Scene t r) = rRender r t

destroyScene :: Scene -> IO ()
destroyScene (Scene _ r) = rCleanup r
destroyScene (SceneContainer _ ss) = mapM_ destroyScene ss

{-# LANGUAGE DeriveGeneric #-}
module UI.Box where

import Types.Internal
import Types.Renderable
import Linear
import Graphics.GL.Core33
import Gelatin.Core.Render
import Control.Eff.Reader.Strict
import Control.Eff.Lift
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)

instance Renderable Box where
    nameOf _ = "Box"
    render box  = do rez <- ask
                     lift $ renderBox rez box >>= return . Just
    transformOf = boxTransform
    renderingHashes b = [hash b]

renderBox :: Rez -> Box -> IO Renderer
renderBox (Rez geom _ _ win _) b@(Box _ (V2 w h) c) = do
    let [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
        vs = [tl, tr, br, tl, br, bl]
        cs = replicate 6 c
    Renderer f c' <- colorRenderer win geom GL_TRIANGLES vs cs
    let c'' = putStrLn $ "Cleaning a box " ++ (show $ hash b)
    return $ Renderer f (c'' >> c')

instance Hashable Box where
    hashWithSalt s (Box _ sz c) = s `hashWithSalt` sz `hashWithSalt` c

data Box = Box { boxTransform :: Transform
               , boxSize      :: Size
               , boxColor     :: Color
               } deriving (Show, Eq, Typeable, Generic)

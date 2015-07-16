{-# LANGUAGE DeriveGeneric #-}
module UI.Box where

import Types.Internal
import Types.Renderable
import Linear
import Graphics.GL.Core33
import Gelatin.Core.Rendering
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)
import Data.IntMap as IM

instance Renderable Box where
    nameOf _ = "Box"
    cache (Rez geom _ _ win _) rs b@(Box _ (V2 w h) c) = do
        let [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
            vs = [tl, tr, br, tl, br, bl]
            cs = replicate 6 c
        r <- colorRendering win geom GL_TRIANGLES vs cs
        putStrLn $ "Cacheing Box " ++ (show $ hash b)
        return $ IM.insert (hash b) r rs
    renderLayerOf b = [(hash b, Just $ boxTransform b)]

instance Hashable Box where
    hashWithSalt s (Box _ sz c) = s `hashWithSalt` sz `hashWithSalt` c

data Box = Box { boxTransform :: Transform
               , boxSize      :: Size
               , boxColor     :: Color
               } deriving (Show, Eq, Typeable, Generic)

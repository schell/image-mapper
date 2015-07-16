{-# LANGUAGE DeriveGeneric #-}
module UI.Picture where

import Types.Internal
import Types.Renderable
import UI.Texture
import Linear
import Gelatin.Core.Rendering
import Graphics.GL.Core33
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.IntMap as IM

instance Renderable Picture where
    nameOf _ = "Pic"
    cache rz@(Rez grs _ _ win _) rs p@(Pic _ _ w' h') = do
        putStrLn "Cacheing a picture"

        let pt = pictureTex p
            [w,h] = map fromIntegral [w',h']
            tl = V2 0 0
            tr = V2 w 0
            bl = V2 0 h
            br = V2 w h
            vs = [tl, tr, bl, tr, bl, br]
            gs = map (/ V2 w h) vs

        Rendering geomf c <- textureRendering win grs GL_TRIANGLES vs gs
        rs' <- cacheIfNeeded rz rs pt

        let texLayer = renderLayerOf pt
            f' t = runLayer texLayer rs' t >> geomf t >> putStrLn "Rendering a pic"
            c'   = (putStrLn $ "Cleaning a picture") >> c
            r    = Rendering f' c'

        return $ IM.insert (hash p) r rs'

    renderLayerOf p =
        renderLayerOf (pictureTex p) ++ [(hash p, Just $ picTransform p)]

pictureTex :: Picture -> Texture
pictureTex (Pic _ p _ _) = Tex p GL_TEXTURE0

instance Hashable Picture where
    hashWithSalt s (Pic _ p w h) =
        s `hashWithSalt` p `hashWithSalt` w `hashWithSalt` h

pictureSize :: Picture -> Size
pictureSize (Pic _ _ w h) = realToFrac <$> V2 w h

data Picture = Pic { picTransform :: Transform
                   , picPath      :: String
                   , picWidth     :: Int
                   , picHeight    :: Int
                   } deriving (Show, Eq, Typeable, Generic)

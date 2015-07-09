{-# LANGUAGE DeriveGeneric #-}
module UI.Picture where

import Types.Internal
import Types.Renderable
import UI.Texture
import Linear
import Gelatin.Core.Render
import Graphics.GL.Core33
import Control.Eff.Reader.Strict
import Control.Eff.Lift
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)

instance Renderable Picture where
    nameOf _ = "Pic"
    render p@(Pic _ _ w' h') = do
        lift $ putStrLn "Compiling a picture"
        (Rez grs _ _ win _) <- ask
        Renderer texf _ <- getRenderer $ pictureTex p
        let [w,h] = map fromIntegral [w',h']
            tl = V2 0 0
            tr = V2 w 0
            bl = V2 0 h
            br = V2 w h
            vs = [tl, tr, bl, tr, bl, br]
            gs = map (/ V2 w h) vs
        Renderer geomf c <- lift $ textureRenderer win grs GL_TRIANGLES vs gs
        let f' t = texf t >> geomf t
            c'   = (putStrLn $ "Cleaning a picture") >> c
        return $ Just $ Renderer f' c'

    transformOf = picTransform

    renderingHashes p = hash p : (renderingHashes $ pictureTex p)

pictureTex :: Picture -> Texture
pictureTex (Pic _ p _ _) = Tex p GL_TEXTURE0

instance Hashable Picture where
    hashWithSalt s (Pic _ p w h) =
        s `hashWithSalt` p `hashWithSalt` w `hashWithSalt` h

data Picture = Pic { picTransform :: Transform
                   , picPath      :: String
                   , picWidth     :: Int
                   , picHeight    :: Int
                   } deriving (Show, Eq, Typeable, Generic)

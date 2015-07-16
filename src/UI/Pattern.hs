{-# LANGUAGE DeriveGeneric #-}
module UI.Pattern where

import Types.Internal
import Types.Renderable
import UI.Texture
import Linear
import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.IntMap as IM

instance Renderable Pattern where
    nameOf _ = "Pattern"
    cache rz@(Rez grs _ _ win _) rs ptrn@(Pattern _ e (Clip ctl cbr) o sz) = do
        putStrLn "Cacheing a pattern"

        let V2 w h = sz
            tl = V2 0 0
            tr = V2 w 0
            bl = V2 0 h
            br = V2 w h
            vs = [tl, tr, bl, tr, bl, br]
            V2 x1 y1 = ctl
            V2 x2 y2 = cbr
            vwh = abs <$> fromIntegral <$> (cbr - ctl)
            texLayer = patternTexLayer ptrn

        rs' <- cacheIfNeeded rz rs e
        tex <- toTexture win $ runLayer (renderLayerOf e) rs' mempty
        wh  <- snd <$> getWindowSize win
        tex' <- clipTexture tex (V2 x1 (wh - y1), V2 x2 (wh - y2))
        cleanTexture tex

        let gs   = map (+ (o/vwh)) $ map (/ vwh) vs
            texr = Rendering (const $ renderTexture GL_TEXTURE0 tex')
                             (cleanTexture tex')
            rs'' = IM.insert (patternTexHash ptrn) texr rs'

        Rendering geomf c <- textureRendering win grs GL_TRIANGLES vs gs

        let f' t = runLayer texLayer rs'' t >> geomf t >> putStrLn "Rendering a pattern"
            c'   = (putStrLn $ "Cleaning a pattern") >> c
            r    = Rendering f' c'

        return $ IM.insert (hash ptrn) r rs''

    renderLayerOf p = [ (hash p, Just $ ptrnTransform p)
                      , (hash $ ptrnElement p, Nothing)
                      ] ++ patternTexLayer p

patternTexLayer :: Pattern -> Layer
patternTexLayer p = [(patternTexHash p, Just mempty)]

patternTexHash :: Pattern -> Int
patternTexHash (Pattern _ e c o _) = hash e `hashWithSalt` c `hashWithSalt` o

instance Hashable Pattern where
    hashWithSalt s (Pattern _ e c o sz) =
        s `hashWithSalt` elementLayer `hashWithSalt` c `hashWithSalt` o `hashWithSalt` sz
            where elementLayer = renderLayerOf e

data Pattern = Pattern { ptrnTransform :: Transform
                       , ptrnElement   :: Element
                       , ptrnClip      :: Clip
                       , ptrnOffset    :: Position
                       , ptrnSize      :: Size
                       } deriving (Show, Eq, Typeable, Generic)

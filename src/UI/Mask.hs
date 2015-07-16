{-# LANGUAGE DeriveGeneric #-}
module UI.Mask where

import Types.Internal
import Types.Renderable
import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Data.Hashable
import Data.Typeable
import GHC.Generics (Generic)
import qualified Data.IntMap as IM

instance Renderable Mask where
    nameOf _ = "Mask"
    cache rz@(Rez _ _ mrs w _) rs m@(Mask _ a b) = do
        putStrLn "Cacheing Mask"
        rs'    <- cacheIfNeeded rz rs a
        rs''   <- cacheIfNeeded rz rs' b
        let layerA = renderLayerOf a
            layerB = renderLayerOf b
        putStrLn $ "layerA: " ++ show layerA
        putStrLn $ "layerB: " ++ show layerB
        r <- renderMask w mrs (runLayer layerA rs'' mempty)
                              (runLayer layerB rs'' mempty)
        return $ IM.insert (hash m) r rs''
    renderLayerOf m@(Mask _ a b) = (hash m, Just $ maskTfrm m) : hidden
        where hidden = map (fmap $ const Nothing) $ concatMap renderLayerOf [a,b]


renderMask :: Window -> MaskRenderSource -> IO () -> IO () -> IO Rendering
renderMask = alphaMask

instance Hashable Mask where
    hashWithSalt s (Mask _ mn msk) = s `hashWithSalt` layer
            -- A mask must re-cache its rendering whenever any part of its
            -- elements' render layers change.
            where layer = concatMap renderLayerOf [mn,msk]

data Mask = Mask { maskTfrm :: Transform
                 , maskMain :: Element
                 , maskMask :: Element
                 } deriving (Show, Eq, Typeable, Generic)

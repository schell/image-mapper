{-# LANGUAGE DeriveGeneric #-}
module UI.Mask where

import Types.Internal
import Types.Renderable
import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Data.Hashable
import Data.Typeable
import GHC.Generics (Generic)
import Data.IntMap as IM

instance Renderable Mask where
    nameOf _ = "Mask"
    transformOf = maskTfrm

    cache rz@(Rez _ _ mrs w _) rs m@(Mask _ a b) = do
        putStrLn "Cacheing Mask"
        rs'  <- cacheRenderings rz rs a
        rs'' <- cacheRenderings rz rs' b
        r    <- renderMask w mrs (runRendering a (transformOf a) rs'')
                                 (runRendering b (transformOf b) rs'')
        return $ IM.insert (hash m) r rs''
    children _ = []
    hashes m@(Mask _ a b) = hash m : concat [hashes a, hashes b]

renderMask :: Window -> MaskRenderSource -> IO () -> IO () -> IO Rendering
renderMask = alphaMask

instance Hashable Mask where
    hashWithSalt s (Mask _ mn msk) =
        s `hashWithSalt` (tmn, mn) `hashWithSalt` (tmsk, msk)
            where tmn = transformOf mn
                  tmsk = transformOf msk

data Mask = Mask { maskTfrm :: Transform
                 , maskMain :: Element
                 , maskMask :: Element
                 } deriving (Show, Eq, Typeable, Generic)

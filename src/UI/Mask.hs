{-# LANGUAGE DeriveGeneric #-}
module UI.Mask where

import Types.Internal
import Types.Renderable
import Gelatin.Core.Render
import Graphics.UI.GLFW
import Control.Eff.Reader.Strict
import Control.Eff.Lift
import Data.Hashable
import Data.Typeable
import GHC.Generics (Generic)

instance Renderable Mask where
    nameOf _ = "Mask"
    transformOf = maskTfrm

    render m@(Mask _ a b) = do
        lift $ putStrLn $ "Compiling a mask " ++ (show $ hash m)
        Rez _ _ mrs w _ <- ask
        Renderer fa _ <- getRenderer a
        Renderer fb _ <- getRenderer b
        Renderer f c  <- lift $ renderMask w mrs (fa $ transformOf a) (fb $ transformOf b)
        let c' = putStrLn $ "Cleaning a mask " ++ (show $ hash m)
        return $ Just $ Renderer f (c' >> c)

    renderingHashes m@(Mask _ a b) = hash m : concatMap renderingHashes [a, b]

renderMask :: Window -> MaskRenderSource -> IO () -> IO () -> IO Renderer
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

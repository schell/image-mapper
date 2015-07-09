{-# LANGUAGE DeriveGeneric #-}
module UI.Button where

import Types.Renderable
import Types.Internal
import Linear
import UI.Box
import UI.Label
import Gelatin.Core.Render
import Control.Varying
import Data.Typeable
import Data.Hashable
import Data.Monoid
import GHC.Generics (Generic)

data ButtonState = ButtonStateOff | ButtonStateHover | ButtonStateOn
data ButtonGraph m = ButtonGraph { buttonGraphBtn   :: Var m InputEvent Button
                                 , buttonGraphPoly  :: Var m InputEvent [V2 Float]
                                 , buttonGraphState :: Var m InputEvent ButtonState
                                 }

instance Renderable Button where
    nameOf _ = "Button"
    render btn@(Button _ box label) = do
        b <- getRenderer box
        l <- getRenderer label
        let f t = do rRender b (t <> transformOf box)
                     rRender l (t <> transformOf label)
            c = putStrLn $ "Cleaning a button " ++ (show $ hash btn)
        return $ Just $ Renderer f c

    transformOf = buttonTransform

    renderingHashes btn@(Button _ a b) =
        hash btn : concatMap renderingHashes [Element a, Element b]

instance Hashable Button where
    hashWithSalt s (Button _ b l) = s `hashWithSalt` b `hashWithSalt` l

data Button = Button { buttonTransform :: Transform
                     , buttonBox       :: Box
                     , buttonLabel     :: Label
                     } deriving (Show, Eq, Typeable, Generic)

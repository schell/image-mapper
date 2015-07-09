{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UI.Label where

import Types.Internal
import Types.Renderable
import Gelatin.Core.Render
import Graphics.Text.TrueType
import Control.Eff.Reader.Strict
import Control.Eff.Lift
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)

instance Renderable Label where
    nameOf _ = "Label"
    -- | TODO: Include one font so we have some default text rendering
    -- immediately.
    render (Label _ _ Nothing _ _) = return Nothing
    render (Label _ str (Just fp) ps fc) = do
        (Rez grs brs _ w _) <- ask
        eFont <- lift $ loadFontFile fp
        case eFont of
            Left err -> lift $ putStrLn err >> return Nothing
            Right fn -> do
                let fstr = FontString fn px (0,0) str
                    px = getPointSize ps
                Renderer f c <- lift $ colorFontRenderer w grs brs fstr $ const fc
                let c' = putStrLn $ "Cleaning a label '" ++ str ++ "'"
                return $ Just $ Renderer f (c' >> c)
    transformOf = labelTransform
    renderingHashes l = [hash l]

instance Hashable PointSize

instance Hashable Label where
    hashWithSalt s (Label _ st fn p c) =
        s `hashWithSalt` st `hashWithSalt` fn `hashWithSalt` p `hashWithSalt` c

data Label = Label { labelTransform      :: Transform
                   , labelString         :: String
                   , labelFontPath       :: Maybe String
                   , labelFontPointSize  :: PointSize
                   , labelColor          :: Color
                   } deriving (Show, Eq, Typeable, Generic)

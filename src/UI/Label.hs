{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UI.Label where

import Types.Internal
import Types.Renderable
import Gelatin.Core.Rendering
import Graphics.Text.TrueType
import Data.Typeable
import Data.Hashable
import Data.IntMap as IM
import GHC.Generics (Generic)

instance Renderable Label where
    nameOf _ = "Label"
    -- | TODO: Include one font so we have some default text rendering
    -- immediately.
    cache _ rs (Label _ _ Nothing _ _) = return rs
    cache (Rez grs brs _ w _) rs l@(Label _ str (Just fp) ps fc) = do
        putStrLn $ "Cacheing Label"
        eFont <- loadFontFile fp
        case eFont of
            Left err -> do putStrLn err
                           return $ IM.insert (hash l) mempty rs
            Right fn -> do
                let fstr = FontString fn px (0,0) str
                    px = getPointSize ps
                Rendering f c <- colorFontRendering w grs brs fstr $ const fc
                let c' = putStrLn $ "Cleaning a label '" ++ str ++ "'"
                    r  = Rendering f (c' >> c)
                return $ IM.insert (hash l) r rs
    renderLayerOf (Label _ _ Nothing _ _) = []
    renderLayerOf l = [(hash l, labelTransform l)]

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

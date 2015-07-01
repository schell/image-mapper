{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Network.MappingScreen where

import Control.Varying
import Control.Eff
import Control.Eff.Lift
import UI.Types
import Scene
import System
import Network.System

mappingScreen :: SetMember Lift (Lift IO) r => Vareff r InputEvent [Element]
mappingScreen = mappingArea

mappingArea :: (SetMember Lift (Lift IO) r) => Vareff r InputEvent [Element]
mappingArea = sequenceA [currentPicture]

currentPicture :: DoesIO r => Vareff r InputEvent Element
currentPicture =
    startingWith anEmptyElement <~ (Element <$>) <$> (tagM (lift . loadPicture) fileDropped)

mappingBackground :: (DoesIO r, ReadRez r) => Vareff r InputEvent Element
mappingBackground = windowSize ~> a box that is less than the window size

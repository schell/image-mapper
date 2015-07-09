{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module System where

import UI.Picture
import Prelude hiding (sequence_)
import Codec.Picture
import Codec.Picture.Types

loadPicture :: String -> IO (Maybe Picture)
loadPicture fp = do
    eStrOrImg <- readImage fp
    case eStrOrImg of
        Left err -> putStrLn err >> return Nothing
        Right i  -> do let w = fromIntegral $ dynamicMap imageWidth i :: Float
                           h = fromIntegral $ dynamicMap imageHeight i :: Float
                       let p = Pic mempty fp (floor w) (floor h)
                       return $ Just p

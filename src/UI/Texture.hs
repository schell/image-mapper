{-# LANGUAGE DeriveGeneric #-}
module UI.Texture where

import Graphics.GL.Core33
import Types.Renderable
import Gelatin.Core.Rendering
import Graphics.GL.Types
import Foreign.Marshal.Array (withArray)
import Codec.Picture
import Data.Typeable
import Data.Hashable
import Data.IntMap as IM
import GHC.Generics (Generic)

instance Renderable Texture where
    nameOf _ = "Texture"
    cache _ rs tex@(Tex path u) = do
        putStrLn $ "Compiling a texture " ++ show path
        eStrOrImg <- readImage path
        case eStrOrImg of
            Left err -> putStrLn err >> return mempty
            Right i  -> do t <- do putStrLn $ "Creating tex for " ++ path
                                   loadTextureUnit (Just u) i
                           let f _ = do glActiveTexture u
                                        glBindTexture GL_TEXTURE_2D t
                               c = do putStrLn $ "Deleting tex for " ++ path
                                      withArray [t] $ glDeleteTextures 1
                           return $ IM.insert (hash tex) (Rendering f c) rs
    transformOf _ = mempty
    children _ = []
    hashes t = [hash t]

instance Hashable Texture

data Texture = Tex { texPath :: String
                   , texUnit :: GLuint
                   } deriving (Show, Eq, Typeable, Generic)

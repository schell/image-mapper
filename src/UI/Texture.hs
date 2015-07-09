{-# LANGUAGE DeriveGeneric #-}
module UI.Texture where

import Graphics.GL.Core33
import Types.Renderable
import Gelatin.Core.Render
import Graphics.GL.Types
import Foreign.Marshal.Array (withArray)
import Codec.Picture
import Control.Eff.Lift
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)

instance Renderable Texture where
    nameOf _ = "Texture"
    render (Tex path u) = do
        lift $ putStrLn $ "Compiling a texture " ++ show path
        eStrOrImg <- lift $ readImage path
        case eStrOrImg of
            Left err -> lift $ putStrLn err >> return mempty
            Right i  -> do t <- lift $ do putStrLn $ "Creating tex for " ++ path
                                          loadTextureUnit (Just u) i
                           let f _ = do glActiveTexture u
                                        glBindTexture GL_TEXTURE_2D t
                               c = do putStrLn $ "Deleting tex for " ++ path
                                      withArray [t] $ glDeleteTextures 1
                           return $ Just $ Renderer f c
    transformOf _ = mempty
    renderingHashes t = [hash t]

instance Hashable Texture

data Texture = Tex { texPath :: String
                   , texUnit :: GLuint
                   } deriving (Show, Eq, Typeable, Generic)

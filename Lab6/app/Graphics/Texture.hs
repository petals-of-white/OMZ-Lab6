module Graphics.Texture (retrieveInfo)
where

import           AppState
import           Control.Monad.Exception (MonadAsyncException)
import           Control.Monad.IO.Class
import           Data.Word
import           Graphics.GPipe

retrieveInfo :: (MonadIO m, MonadAsyncException m, ContextHandler ctx) =>
    Texture2D os (Format RWord) -> V2 Int -> ContextT ctx os m (Bounds Word16, Maybe Word16)

retrieveInfo tex _point@(V2 x y) =
    (\(b,eitherPix) -> (b, case eitherPix of Right pix -> Just pix; Left _ -> Nothing))
    <$>
    readTexture2D tex 0 0 (head (texture2DSizes tex)) accum initial

    where
        initial = (Bounds {minValue=maxBound::Word16, maxValue=minBound :: Word16}, Left 0)
        flatPos = y*texW + x
        V2 texW _texH = head (texture2DSizes tex)
        accum _acc@(Bounds{minValue,maxValue}, pos) (pixel :: Word16) =
            let newBounds = Bounds {minValue=min minValue pixel, maxValue=max maxValue pixel}
                pixPos =
                    case pos of
                        Left p | p == flatPos -> Right pixel
                        Left p | otherwise    -> Left (p+1)
                        Right found           -> Right found
            in
            return (newBounds, pixPos)


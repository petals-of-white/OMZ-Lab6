module Input (
    coordsUnderCursor,
    keyModeChanged, mapCursorCoordinates
) where

import           AppState
import           Control.Monad.Exception     (MonadAsyncException)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.List                   (uncons)
import           Data.Maybe                  (catMaybes)
import           Graphics.GPipe
import           Graphics.GPipe.Context.GLFW as GLFW


keyModeChanged :: MonadIO m => Window os RGBFloat ds -> ContextT Handle os m (Maybe ImageMode)

keyModeChanged win = do
    orig <- getKey win Key'O
    bin <- getKey win Key'B
    winlevel <- getKey win Key'W
    normlz <- getKey win Key'N
    return $ fmap fst $ uncons $ catMaybes
        [(if keyState == Just KeyState'Pressed then Just format else Nothing) |
         (format, keyState) <-
            [Original,Binarized,WindowLevel,AppState.Normalized] `zip` [orig,bin,winlevel,normlz]]


coordsUnderCursor :: (MonadIO m, MonadAsyncException m) => Window os RGBFloat () -> ViewPort -> ContextT Handle os m (V2 Int)

coordsUnderCursor win viewport  = do
    fbSize <- getFrameBufferSize win
    maybeCursPos <- GLFW.getCursorPos win
    return $
        case maybeCursPos of
            Just (x,y) -> mapCursorCoordinates (V2 x y) fbSize viewport
            Nothing    -> 0


mapCursorCoordinates :: V2 Double -> V2 Int -> ViewPort -> V2 Int

mapCursorCoordinates _glfwCursPos@(V2 cursX cursY) _winSize@(V2 _ wh) ViewPort {viewPortLowerLeft = (V2 vw0 vh0), viewPortSize=vpSize} =

    let s = fromIntegral <$> V2 vw0 (wh - vh0)
        sus = V2 1 (-1) * (V2 cursX cursY - s) in

    box (V2 0 0) (vpSize -1) (fmap floor sus)
    where
            box (V2 x1 y1) (V2 x2 y2) (V2 x y) = V2 (clip x1 x2 x) (clip y1 y2 y)
            clip left right x | x < left = left
                            | x > right = right
                            | otherwise = x

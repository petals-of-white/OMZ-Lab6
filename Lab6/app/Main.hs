module Main where
import           AppState
import           Control.Monad.Except
import           Data.DICOM           as Dicom
import           Data.DICOM.Utilities
import           Data.Either
import           Graphics.GPipe
import           Graphics.Render      as Render
import           Prelude              hiding (reverse)
import           System.Environment   (getArgs)


main :: IO ()
main = do
    dicomPath:other <- getArgs
    let [left,right,binThreshold,divideBy] = fmap read other
        transformationOptions =
          (Windowing {ww = right - left, wl = (left + right) / 2}, BinTreshold binThreshold, Bounds left right, 1.0/divideBy)

    dicom <- either error id <$> readObjectFromFile dicomPath
    let elemMap = toMap dicom
        metadata = do
          r <- rows elemMap
          c <- columns elemMap
          intercept <- rescaleIntercept elemMap
          slope <- rescaleSlope elemMap
          bitsAlloc <- bitsAllocated elemMap
          imgBytes <- pixelData elemMap
          return (r,c, imgBytes, intercept, slope, bitsAlloc)

    case metadata of
      Right (row, col, imgBytes, intercept, slope, bitsAlloc) -> do
        putStrLn $ "Rows: " ++ show row ++ ". Columns: " ++ show col ++ ". Intercept: " ++
         show intercept ++ ". Slope: " ++ show slope ++ ". Bits allocated: " ++ show bitsAlloc
        case (intercept, slope, bitsAlloc) of
          (i, s, _) | i /= 0, s /= 0 -> do
            putStrLn "gl float."
            error "Float textures are currently not supported."
          (_,_, 8)                   -> do
            putStrLn "gl byte"
            Render.app R8UI (V2 (fromIntegral row) (fromIntegral col)) imgBytes transformationOptions
          (_,_, 16)                  -> do
            putStrLn "gl short"
            Render.app R16UI (V2 (fromIntegral row) (fromIntegral col)) imgBytes transformationOptions
          _                          ->
            error "Unrecognized pixel type."

      Left _ -> error "Error reading metadata!"

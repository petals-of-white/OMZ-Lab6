{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           App
import           Data.Binary          as Binary (byteSwap16, decode, encode)
import qualified Data.ByteString.Lazy as L
import           Data.DICOM           as DICOM
import           Data.Word            (Word16)
import           Monomer
import           System.Environment   (getArgs)



main :: IO ()
main = do
  args <- getArgs
  case args of
    dicomPath:_ -> do
      dicomObj <- DICOM.readObjectFromFile dicomPath

      case dicomObj of

        Right dicom ->

          let dicomMap = DICOM.toMap dicom
              eitherInfo = do
                      pixBytes <- DICOM.pixelData dicomMap
                      rows_ <- DICOM.rows dicomMap
                      cols <- DICOM.columns dicomMap
                      intercept <- DICOM.rescaleIntercept dicomMap
                      slope <- DICOM.rescaleSlope dicomMap
                      bitsAlloc <- DICOM.bitsAllocated dicomMap
                      return (pixBytes, fromIntegral rows_, fromIntegral cols, intercept, slope, bitsAlloc)
          in

          case eitherInfo of
            Right (pixByteString, row, col, intercept, slope, bitsAlloc) ->
              case (intercept, slope, bitsAlloc) of
                    (i, s, _) | i /= 0, s /= 0 -> putStrLn "Float"
                    (_,_, 8)                   -> putStrLn "Byte"
                    (_,_, 16)                  ->

                      let dicomPixels :: [Word16] =
                            byteSwap16 <$> decode (L.append (encode (row*col :: Int)) (L.fromStrict pixByteString))

                          model = AppModel {
                            translateOyPos = 0, rotateClockwiseDeg = 0,
                            imgPixels = dicomPixels,
                            imgRows=row,
                            imgCols=col
                            }

                          config = [
                                  appWindowTitle "Lab6",

                                  appWindowIcon "./assets/images/icon.png",
                                  appWindowResizable False,
                                  appTheme darkTheme,
                                  appWindowState (MainWindowNormal (row*2, col*2)),
                                  appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
                                  appInitEvent AppInit
                                  ]
                      in putStrLn "Short"  >>
                         startApp model handleEvent buildUI config
                    _ -> error "Unrecorgnized pixel data type"



            Left err -> error (show err)

        Left err -> error err

    [] -> error "No arguments. Pass dicom path"

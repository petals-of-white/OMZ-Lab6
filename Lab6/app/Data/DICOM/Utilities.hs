module Data.DICOM.Utilities where

import           Data.Binary           as Binary (decodeOrFail)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSChar
import           Data.ByteString.Lazy  as LBS (fromStrict)
import           Data.DICOM            as DicomList
import           Data.List             as List (delete)
import           Data.Map              as Map
import           Data.Word
import           Text.Read             (readEither)

type Intercept = Float
type Slope = Float
type Size = (Int, Int)
type BitsAllocated = Word16


type ElementMap = Map Tag (VL, VR, ElementContent)

data ElementError = NotFound Tag String | ParseError Tag String | ContentError Tag ElementContent

bytesContent :: ElementContent -> Maybe BS.ByteString
bytesContent (BytesContent content) = Just content
bytesContent _                      = Nothing

elContent :: (VL, VR, ElementContent) -> ElementContent
elContent (_,_,content) = content

toMap :: Object -> ElementMap
toMap = Map.fromList . fmap (\Element {elementTag, elementVL, elementVR, elementContent} -> (elementTag, (elementVL, elementVR, elementContent))) . runObject

extractValue :: ElementMap -> Tag -> String -> (BS.ByteString -> Either String a) -> Either ElementError a
extractValue elemMap tag label f = do
      entry <- maybeToEither (NotFound tag label) (elemMap !? tag)
      content <- maybeToEither (ContentError tag (elContent entry)) $ bytesContent (elContent entry)

      mapLeft (ParseError tag) $ f content

rows :: ElementMap -> Either ElementError Word16
rows elemMap = do
      extractValue elemMap Rows "Rows"
            (fmap (\(_, _, v) -> v)  . mapLeft (\(_,_,msg) -> msg) . decodeOrFail . LBS.fromStrict .  BS.reverse)

columns :: ElementMap -> Either ElementError Word16
columns elemMap = do
      extractValue elemMap Columns "Columns"
            (fmap (\(_, _, v) -> v)  .  mapLeft (\(_,_,msg) -> msg) . decodeOrFail . LBS.fromStrict .  BS.reverse)


pixelData :: ElementMap -> Either ElementError BS.ByteString
pixelData elemMap =
      extractValue elemMap PixelData "PixelData" pure


rescaleIntercept :: ElementMap -> Either ElementError Slope
rescaleIntercept elemMap =
      extractValue elemMap RescaleIntercept "Rescale Intercept" (readEither . List.delete '+' . BSChar.unpack)

rescaleSlope :: ElementMap -> Either ElementError Slope
rescaleSlope elemMap =
      extractValue elemMap RescaleSlope "Rescale Slope" (readEither . List.delete '+' . BSChar.unpack)


bitsAllocated :: ElementMap -> Either ElementError BitsAllocated
bitsAllocated elemMap =
      extractValue elemMap BitsAllocated "Bits Allocated"
            (fmap (\(_, _, v) -> v)  .  mapLeft (\(_,_,msg) -> msg) . decodeOrFail . LBS.fromStrict . BS.reverse)


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft _ (Right v) = Right v
mapLeft f (Left e)  = Left (f e)

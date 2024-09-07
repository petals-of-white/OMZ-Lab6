{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module App where

import           Data.Convertible (Convertible)
import           Data.Data        (Typeable)
import           Foreign          (Storable)
import           Linear
import           Monomer
import           OpenGLWidgetNew

data AppModel p = AppModel {
  translateOyPos     :: Int,
  rotateClockwiseDeg :: Int,
  imgPixels          :: [p],
  imgRows            :: Int,
  imgCols            :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit | TranslateOyPosChanged Int | RotateClockwiseDeg Int
  deriving (Eq, Show)

-- | Градуси на радіани
degToRad :: Float -> Float
degToRad deg = deg * pi / 180

-- | 2д обертання навколо центру координати за годинниковою стрілкою
rotationMatrix :: Float -> M44 Float
rotationMatrix degrees =
      V4  (V4 cosTh (-sinTh)  0 0)
          (V4 sinTh cosTh     0 0)
          (V4 0     0         1 0)
          (V4 0     0         0 1)

  where sinTh = sin (degToRad (-degrees))
        cosTh = cos (degToRad (-degrees))

-- | Зсув за вісcю в додатному напряму
translationMatrix :: Float -> Int -> M44 Float
translationMatrix scale nPixels =
       V4 (V4 1 0 0 0)
          (V4 0 1 0 ty)
          (V4 0 0 1 0)
          (V4 0 0 0 1)
  where ty = fromIntegral nPixels / scale


buildUI
  :: (Storable p, Show p, Typeable p, Bounded p, Real p, Convertible Double p) =>
  WidgetEnv (AppModel p)  AppEvent
  -> AppModel p
  -> WidgetNode (AppModel p) AppEvent
  
buildUI _wenv AppModel {translateOyPos=ty, rotateClockwiseDeg=rotDeg, imgPixels=pixels, imgCols=cols, imgRows=rows} = widgetTree where
  widgetTree = vstack [
      hstack [
        label "Трансляція вздовж  Oy (додатний напрямок):",
        spacer,
        numericFieldV_ ty TranslateOyPosChanged [minValue 0, maxValue rows]
      ],
      hstack
      [
        label "Обертання за годинниковою стрілкою відносно поч. коорд.",
        spacer,
        numericFieldV_ rotDeg RotateClockwiseDeg [minValue 0, maxValue 360]
      ],

      box (openGLWidget pixels rows cols transMat)

    ]

  transMat = rotMat !*! translateMat

  rotMat = rotationMatrix (fromIntegral rotDeg)
  translateMat = translationMatrix (fromIntegral rows) ty


handleEvent
  :: WidgetEnv (AppModel p) AppEvent
  -> WidgetNode (AppModel p) AppEvent
  -> AppModel p
  -> AppEvent
  -> [AppEventResponse (AppModel p) AppEvent]



handleEvent _wenv _node model evt = case evt of
  TranslateOyPosChanged y -> [Model model {translateOyPos = y}, Request RenderOnce]
  RotateClockwiseDeg deg  -> [Model model {rotateClockwiseDeg = deg}, Request RenderOnce]
  AppInit                 -> []

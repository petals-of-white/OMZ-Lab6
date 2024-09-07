module AppState
    (State(..), ImageInfo(..), Windowing(..), BinTreshold(..), ImageMode(..), Scaling, TransOptions, Bounds(..))
 where


-- | Includes lab assignment histogram transformation 'modes'
data ImageMode = Original | Binarized | WindowLevel | Normalized  deriving (Eq, Show)

-- | Represents a state of an entrire application every frame
data State i = State {
    imgMode       :: ImageMode,
    transOptions  :: TransOptions i,
    originalPeaks :: Bounds i
}
-- | Used to represent some kind of bounds or peaks
data Bounds p = Bounds {minValue :: p, maxValue :: p} deriving Show


data ImageInfo p = ImageInfo {
    currentPeaks :: Bounds p,
    transState   :: State p,
    hoveredPixel :: Maybe p}

type NormalizationRange p = Bounds p
-- | Transformation options, according to lab assingment
type TransOptions i = (Windowing i, BinTreshold i, NormalizationRange i,  Scaling)

-- | Scaling is just a float
type Scaling = Float

-- | Window-level transformation
data Windowing i = Windowing {
    ww :: i, -- ^ window width
    wl :: i  -- ^ window level
} deriving (Show, Eq)


newtype BinTreshold i = BinTreshold i deriving (Show , Eq)

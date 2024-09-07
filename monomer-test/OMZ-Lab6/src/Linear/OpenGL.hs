module Linear.OpenGL where

import           Control.Lens
import           Foreign.Ptr
import           Foreign.Storable

import           Graphics.Rendering.OpenGL.GL as GL hiding (color)
import           Linear
import           System.IO.Unsafe

glMatrixToM44 :: MatrixComponent a => GLmatrix a -> IO (M44 a)
glMatrixToM44 m = withMatrix m $ \order p ->
    traverse (traverse $ peekElemOff p) (go order)
  where
    go RowMajor =
        V4 (V4  0  1  2  3)
           (V4  4  5  6  7)
           (V4  8  9 10 11)
           (V4 12 13 14 15)
    go ColumnMajor =
        V4 (V4  0  4  8 12)
           (V4  1  5  9 13)
           (V4  2  6 10 14)
           (V4  3  7 11 15)


m44ToGLmatrix :: MatrixComponent a => M44 a -> IO (GLmatrix a)
m44ToGLmatrix m = withNewMatrix RowMajor $ \p->poke (castPtr p) m


-- | An isomorphism between GL and linear four-dimensional matrices
m44GLmatrix :: MatrixComponent a => Iso' (M44 a) (GLmatrix a)
m44GLmatrix = iso (unsafePerformIO . m44ToGLmatrix) (unsafePerformIO . glMatrixToM44)


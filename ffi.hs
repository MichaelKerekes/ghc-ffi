------------------------------------------------------------------------------
--
--  Core.FFI
--
------------------------------------------------------------------------------

module Core.FFI (
  reference,
  apply0,
  apply1,
  apply2,
  apply3,
  apply4,
  apply5,
  apply6,
  CBool,
  ToC(..),
  FromC(..),
) where

import Core
import Core.IO
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr

type CBool = CChar

------------------------------------------------------------------------------
--  Reference counted foreign pointers
------------------------------------------------------------------------------

foreign import ccall unsafe "&mrtRelease" mrtRelease :: FunPtr (Ptr a -> IO ())

reference :: Ptr a -> IO (ForeignPtr a)
reference x = newForeignPtr mrtRelease x

------------------------------------------------------------------------------
--  ToC
------------------------------------------------------------------------------

class ToC a b where
  toC :: a -> (b -> IO c) -> IO c

instance ToC Bool CBool where
  toC False f = f 0
  toC True  f = f 1

instance ToC Int CInt where
  toC x f = f $ fromIntegral x

instance ToC Float CFloat where
  toC x f = f $ CFloat x

instance ToC Double CDouble where
  toC x f = f $ CDouble x

instance ToC [Char] CString where
  toC = withCString

instance ToC [Char] CWString where
  toC = withCWString

instance ToC (ForeignPtr a) (Ptr a) where
  toC = withForeignPtr

------------------------------------------------------------------------------
--  FromC
------------------------------------------------------------------------------

class FromC a b where
  fromC :: IO a -> IO b

instance FromC () () where
  fromC = id

instance FromC CBool Bool where
  fromC iox = do
    x <- iox
    return (x /= 0)

instance FromC CInt Int where
  fromC iox = map fromIntegral iox

instance FromC CFloat Float where
  fromC iox = do
    x <- iox
    return $ realToFrac x

instance FromC CDouble Double where
  fromC iox = do
    x <- iox
    return $ realToFrac x

instance FromC (Ptr a) (ForeignPtr a) where
  fromC px = px >>= reference

instance FromC (Ptr a) (Maybe (ForeignPtr a)) where
  fromC px = do
    p <- px
    if p == nullPtr then return None else Some <$> reference p

------------------------------------------------------------------------------
--  ApplyC
------------------------------------------------------------------------------

apply0 x = fromC x

apply1 f x0 = toC x0 $ \p0 -> fromC $ f p0

apply2 f x0 x1 =
  toC x0 $ \p0 ->
    toC x1 $ \p1 ->
      fromC $ f p0 p1

apply3 f x0 x1 x2 =
  toC x0 $ \p0 ->
    toC x1 $ \p1 ->
      toC x2 $ \p2 ->
        fromC $ f p0 p1 p2

apply4 f x0 x1 x2 x3 =
  toC x0 $ \p0 ->
    toC x1 $ \p1 ->
      toC x2 $ \p2 ->
        toC x3 $ \p3 ->
          fromC $ f p0 p1 p2 p3


apply5 f x0 x1 x2 x3 x4 =
  toC x0 $ \p0 ->
    toC x1 $ \p1 ->
      toC x2 $ \p2 ->
        toC x3 $ \p3 ->
          toC x4 $ \p4 ->
            fromC $ f p0 p1 p2 p3 p4

apply6 f x0 x1 x2 x3 x4 x5 =
  toC x0 $ \p0 ->
    toC x1 $ \p1 ->
      toC x2 $ \p2 ->
        toC x3 $ \p3 ->
          toC x4 $ \p4 ->
            toC x5 $ \p5 ->
              fromC $ f p0 p1 p2 p3 p4 p5

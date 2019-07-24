------------------------------------------------------------------------------
--
--  MRT
--
------------------------------------------------------------------------------

module Media.MRT (
  initialize,
  uninitialize,
  DeviceContext,
  Texture, loadTexture, textureGetSize,
  Font, createFont, createFixedFont, fontNormalWeight, textBound,
  PixelShader, createPixelShader,
  PipelineState,
  Engine, createEngine,
  setPipelineState,
  setColor,
  pushState,
  popState,
  translate,
  scale,
  rotate,
  transform,
  clip,
  beginFrame,
  endFrame,
  getTrianglesPerDraw,
  drawTexture,
  fillRectangle,
  drawText,
  drawTest,
  KeyCode(..), toChar,
  Window, openWindow,
  WindowMessage(..),
  PointInt(..),
  closeWindow,
  windowClosed,
  getDeviceContext,
  handleInput,
  getTime, {-timeIO,-}
) where

------------------------------------------------------------------------------
--  Imports
------------------------------------------------------------------------------

import Core
import Core.FFI
import Data.Char
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Media.Data (PointInt(..), Vector(..), Rect(..), Color(..))

------------------------------------------------------------------------------
--  Types
------------------------------------------------------------------------------

data CDeviceContext
data CTexture
data CFont
data CText
data CPixelShader
data CPipelineState
data CEngine
data CWindow

type PDeviceContext = Ptr CDeviceContext
type PTexture       = Ptr CTexture
type PFont          = Ptr CFont
type PText          = Ptr CText
type PPixelShader   = Ptr CPixelShader
type PPipelineState = Ptr CPipelineState
type PEngine        = Ptr CEngine
type PWindow        = Ptr CWindow

type PInt           = Ptr CInt
type PColor         = Ptr Color
type PVector        = Ptr Vector
type PRect          = Ptr Rect
type PPointInt      = Ptr PointInt

type DeviceContext = ForeignPtr CDeviceContext
type Texture       = ForeignPtr CTexture
type Font          = ForeignPtr CFont
type Text          = ForeignPtr CText
type PixelShader   = ForeignPtr CPixelShader
type PipelineState = ForeignPtr CPipelineState
type Engine        = ForeignPtr CEngine
type Window        = ForeignPtr CWindow

------------------------------------------------------------------------------
--  ToC Storable
------------------------------------------------------------------------------

instance Storable a => ToC a (Ptr a) where
  toC v f =
    alloca $ \pv -> do
      poke pv v
      f pv

------------------------------------------------------------------------------
--  Storable PointInt
------------------------------------------------------------------------------
-- !!! make all this casting automatic

-- data PointInt = PointInt Int Int

instance Storable PointInt where
  sizeOf    _ = 2 * sizeOf (undefined :: CInt)
  alignment _ = alignment  (undefined :: CInt)

  peek p = lift2 PointInt (read 0) (read 1)
    where
      read index = fromIntegral <$> peekElemOff (castPtr p :: Ptr CInt) index

  poke p (PointInt x y) =
    do
      write 0 x
      write 1 y
    where
      write index x = pokeElemOff (castPtr p) index (fromIntegral x :: CInt)

------------------------------------------------------------------------------
--  Storable Vector
------------------------------------------------------------------------------

-- data Vector = Vector Float Float

instance Storable Vector where
  sizeOf    _ = 2 * sizeOf (undefined :: CFloat)
  alignment _ = alignment  (undefined :: CFloat)

  peek p = lift2 Vector (read 0) (read 1)
    where
      read index = realToFrac <$> peekElemOff (castPtr p :: Ptr CFloat) index

  poke p (Vector x y) =
    do
      write 0 x
      write 1 y
    where
      write index x = pokeElemOff (castPtr p) index (CFloat x)

------------------------------------------------------------------------------
--  Storable Rect
------------------------------------------------------------------------------

instance Storable Rect where
  sizeOf    r = 2 * sizeOf (undefined :: Vector)
  alignment r = alignment  (undefined :: Vector)

  peek p = lift2 Rect (read 0) (read 1)
    where
      read index = peekElemOff (castPtr p) index

  poke prect (Rect min max) =
    do
      write 0 min
      write 1 max
    where
      write index x = pokeElemOff (castPtr prect) index x

------------------------------------------------------------------------------
--  Storable Color
------------------------------------------------------------------------------

instance Storable Color where
  sizeOf    _ = 4 * sizeOf (undefined :: CFloat)
  alignment _ = alignment  (undefined :: CFloat)

  peek p = lift4 Color (read 0) (read 1) (read 2) (read 3)
    where
      read index = realToFrac <$> peekElemOff (castPtr p :: Ptr CFloat) index

  poke p (Color r g b a) =
    do
      write 0 r
      write 1 g
      write 2 b
      write 3 a
    where
      write index x = pokeElemOff (castPtr p) index (CFloat x)

------------------------------------------------------------------------------
--  System
------------------------------------------------------------------------------

foreign import ccall safe mrtInitialize   :: IO ()
foreign import ccall safe mrtUninitialize :: IO ()
foreign import ccall safe mrtGetTime      :: IO CDouble

initialize :: IO ()
initialize = apply0 mrtInitialize

uninitialize :: IO ()
uninitialize = apply0 mrtUninitialize

getTime :: IO Double
getTime = apply0 mrtGetTime

timeIO :: IO a -> IO (Double, a)
timeIO io = do
  tstart <- getTime
  x <- io
  tend   <- getTime
  return (tstart - tend, x)

------------------------------------------------------------------------------
--  PixelShader
------------------------------------------------------------------------------

foreign import ccall safe mrtCreatePixelShader :: CString -> CString -> IO PPixelShader

createPixelShader :: String -> String -> IO PixelShader
createPixelShader = apply2 mrtCreatePixelShader

------------------------------------------------------------------------------
--  Texture
------------------------------------------------------------------------------

foreign import ccall safe mrtLoadTexture    :: CWString -> IO PTexture
foreign import ccall safe mrtTextureGetSize :: PTexture -> PPointInt -> IO ()

loadTexture    :: String -> IO Texture
textureGetSize :: Texture -> PointInt

loadTexture = apply1 mrtLoadTexture

-- !!!!! can unsafePerformIO be removed here?
-- !!!!! all the mrt calls are pure

textureGetSize texture =
  unsafePerformIO $
    toC texture $ \ptexture ->
      alloca $ \ppoint -> do
        mrtTextureGetSize ptexture ppoint
        peek ppoint

------------------------------------------------------------------------------
--  Font
------------------------------------------------------------------------------

foreign import ccall safe mrtCreateFont :: CWString -> CFloat -> CFloat -> CBool -> CBool -> IO PFont

createFont :: String -> Float -> Bool -> IO Font
createFont = apply3 $ \face weight bItalic -> do
  pfont <- mrtCreateFont face 1 weight bItalic 1
  if pfont == nullPtr then $fail else return pfont

createFixedFont :: String -> Float -> Float -> Bool -> IO Font
createFixedFont = apply4 $ \face height weight bItalic -> mrtCreateFont face height weight bItalic 0

fontNormalWeight = 0.4 :: Float

------------------------------------------------------------------------------
--  Text
------------------------------------------------------------------------------

foreign import ccall safe mrtTextBound    :: PFont -> CWString -> PRect -> IO ()
foreign import ccall safe mrtHitTest      :: PFont -> CWString -> PVector -> PInt -> PRect -> IO CBool
foreign import ccall safe mrtGetGlyphRect :: PFont -> CWString -> CInt -> PRect -> IO ()
foreign import ccall safe mrtDrawText     :: PEngine -> PFont -> CWString -> IO ()

textBound :: Font -> String -> Rect
textBound font s =
  unsafePerformIO $
    toC font $ \pfont ->
      toC s $ \ps ->
        alloca $ \prect -> do
          mrtTextBound pfont ps prect
          peek prect

-- !!! test
hitTestText :: Font -> String -> Vector -> (Bool, Int, Rect)
hitTestText font string v =
  unsafePerformIO $
    toC font $ \pfont ->
      toC string $ \ps ->
        toC v $ \pv ->
          alloca $ \pindex ->
            alloca $ \prect -> do
              bHit  <- fromC $ mrtHitTest pfont ps pv pindex prect
              index <- fromC $ peekElemOff pindex 0
              rect  <- peek prect
              return $ (bHit, index, rect)

-- !!! test
getGlyphRect :: Font -> String -> Int -> Rect
getGlyphRect font string index =
  unsafePerformIO $
    toC font $ \pfont ->
      toC string $ \ps ->
        toC index $ \cindex ->
          alloca $ \prect -> do
            mrtGetGlyphRect pfont ps cindex prect
            peek prect

drawText :: Engine -> Font -> String -> IO ()
drawText = apply3 mrtDrawText

------------------------------------------------------------------------------
--  Engine
------------------------------------------------------------------------------

foreign import ccall safe mrtCreateEngine        :: PDeviceContext -> IO PEngine
foreign import ccall safe mrtSetPipelineState    :: PEngine -> PPipelineState -> IO ()
foreign import ccall safe mrtSetColor            :: PEngine -> PColor -> IO ()
foreign import ccall safe mrtPushState           :: PEngine -> IO ()
foreign import ccall safe mrtPopState            :: PEngine -> IO ()
foreign import ccall safe mrtTranslate           :: PEngine -> PVector -> IO ()
foreign import ccall safe mrtScale               :: PEngine -> PVector -> IO ()
foreign import ccall safe mrtRotate              :: PEngine -> CFloat -> IO ()
foreign import ccall safe mrtTransform           :: PEngine -> PVector -> IO ()
foreign import ccall safe mrtClip                :: PEngine -> PRect -> IO ()
foreign import ccall safe mrtBeginFrame          :: PEngine -> IO CBool
foreign import ccall safe mrtEndFrame            :: PEngine -> IO ()
foreign import ccall safe mrtGetTrianglesPerDraw :: PEngine -> IO CInt
foreign import ccall safe mrtDrawTexture         :: PEngine -> PTexture -> IO ()
foreign import ccall safe mrtFillRectangle       :: PEngine -> PRect -> IO ()
foreign import ccall safe mrtDrawTest            :: PEngine -> CFloat -> PVector -> IO ()

createEngine        :: DeviceContext -> IO Engine
setPipelineState    :: Engine -> PipelineState -> IO ()
setColor            :: Engine -> Color -> IO ()
pushState           :: Engine -> IO ()
popState            :: Engine -> IO ()
translate           :: Engine -> Vector -> IO ()
scale               :: Engine -> Vector -> IO ()
rotate              :: Engine -> Float -> IO ()
transform           :: Engine -> Vector -> IO ()
clip                :: Engine -> Rect -> IO ()
beginFrame          :: Engine -> IO Bool
endFrame            :: Engine -> IO ()
getTrianglesPerDraw :: Engine -> IO Int
drawTexture         :: Engine -> Texture -> IO ()
fillRectangle       :: Engine -> Rect -> IO ()
drawTest            :: Engine -> Float -> Vector -> IO ()

createEngine        = apply1 mrtCreateEngine
setPipelineState    = apply2 mrtSetPipelineState
setColor            = apply2 mrtSetColor
pushState           = apply1 mrtPushState
popState            = apply1 mrtPopState
translate           = apply2 mrtTranslate
scale               = apply2 mrtScale
rotate              = apply2 mrtRotate
transform           = apply2 mrtTransform
clip                = apply2 mrtClip
beginFrame          = apply1 mrtBeginFrame
endFrame            = apply1 mrtEndFrame
getTrianglesPerDraw = apply1 mrtGetTrianglesPerDraw
drawTexture         = apply2 mrtDrawTexture
fillRectangle       = apply2 mrtFillRectangle
drawTest            = apply3 mrtDrawTest

------------------------------------------------------------------------------
--  KeyCode
------------------------------------------------------------------------------

data KeyCode =
    KeyNone
  | KeyBackspace
  | KeyTab
  | KeyEnter
  | KeyShift
  | KeyControl
  | KeyAlt
  | KeyPause
  | KeyCapsLock
  | KeyEscape
  | KeySpace
  | KeyPageUp
  | KeyPageDown
  | KeyEnd
  | KeyHome
  | KeyLeft
  | KeyUp
  | KeyRight
  | KeyDown
  | KeyInsert
  | KeyDelete
  | KeyNumPad0
  | KeyNumPad1
  | KeyNumPad2
  | KeyNumPad3
  | KeyNumPad4
  | KeyNumPad5
  | KeyNumPad6
  | KeyNumPad7
  | KeyNumPad8
  | KeyNumPad9
  | KeyNumPadMultiply
  | KeyNumPadPlus
  | KeyNumPadEnter
  | KeyNumPadMinus
  | KeyNumPadDecimal
  | KeyNumPadDivide
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyNumLock
  | KeyScrollLock
  | KeyLeftShift
  | KeyRightShift
  | KeyLeftControl
  | KeyRightControl
  | KeyLeftAlt
  | KeyRightAlt
  | KeySemiColon
  | KeyEquals
  | KeyComma
  | KeyMinus
  | KeyPeriod
  | KeyForwardSlash
  | KeyApostrophe
  | KeyLeftSquare
  | KeyBackSlash
  | KeyRightSquare
  | KeyQuote
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  deriving Eq

toChar :: KeyCode -> Maybe Char
toChar Key0 = Some '0'
toChar Key1 = Some '1'
toChar Key2 = Some '2'
toChar Key3 = Some '3'
toChar Key4 = Some '4'
toChar Key5 = Some '5'
toChar Key6 = Some '6'
toChar Key7 = Some '7'
toChar Key8 = Some '8'
toChar Key9 = Some '9'
toChar KeyA = Some 'a'
toChar KeyB = Some 'b'
toChar KeyC = Some 'c'
toChar KeyD = Some 'd'
toChar KeyE = Some 'e'
toChar KeyF = Some 'f'
toChar KeyG = Some 'g'
toChar KeyH = Some 'h'
toChar KeyI = Some 'i'
toChar KeyJ = Some 'j'
toChar KeyK = Some 'k'
toChar KeyL = Some 'l'
toChar KeyM = Some 'm'
toChar KeyN = Some 'n'
toChar KeyO = Some 'o'
toChar KeyP = Some 'p'
toChar KeyQ = Some 'q'
toChar KeyR = Some 'r'
toChar KeyS = Some 's'
toChar KeyT = Some 't'
toChar KeyU = Some 'u'
toChar KeyV = Some 'v'
toChar KeyW = Some 'w'
toChar KeyX = Some 'x'
toChar KeyY = Some 'y'
toChar KeyZ = Some 'z'
toChar _    = None

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

toKeyCode :: CInt -> KeyCode

toKeyCode 0x08 = KeyBackspace
toKeyCode 0x09 = KeyTab
toKeyCode 0x0d = KeyEnter
toKeyCode 0x10 = KeyShift
toKeyCode 0x11 = KeyControl
toKeyCode 0x12 = KeyAlt
toKeyCode 0x13 = KeyPause
toKeyCode 0x14 = KeyCapsLock
toKeyCode 0x1b = KeyEscape
toKeyCode 0x20 = KeySpace
toKeyCode 0x21 = KeyPageUp
toKeyCode 0x22 = KeyPageDown
toKeyCode 0x23 = KeyEnd
toKeyCode 0x24 = KeyHome
toKeyCode 0x25 = KeyLeft
toKeyCode 0x26 = KeyUp
toKeyCode 0x27 = KeyRight
toKeyCode 0x28 = KeyDown
toKeyCode 0x2D = KeyInsert
toKeyCode 0x2E = KeyDelete
toKeyCode 0x60 = KeyNumPad0
toKeyCode 0x61 = KeyNumPad1
toKeyCode 0x62 = KeyNumPad2
toKeyCode 0x63 = KeyNumPad3
toKeyCode 0x64 = KeyNumPad4
toKeyCode 0x65 = KeyNumPad5
toKeyCode 0x66 = KeyNumPad6
toKeyCode 0x67 = KeyNumPad7
toKeyCode 0x68 = KeyNumPad8
toKeyCode 0x69 = KeyNumPad9
toKeyCode 0x6a = KeyNumPadMultiply
toKeyCode 0x6b = KeyNumPadPlus
toKeyCode 0x6c = KeyNumPadEnter
toKeyCode 0x6d = KeyNumPadMinus
toKeyCode 0x6e = KeyNumPadDecimal
toKeyCode 0x6f = KeyNumPadDivide
toKeyCode 0x70 = KeyF1
toKeyCode 0x71 = KeyF2
toKeyCode 0x72 = KeyF3
toKeyCode 0x73 = KeyF4
toKeyCode 0x74 = KeyF5
toKeyCode 0x75 = KeyF6
toKeyCode 0x76 = KeyF7
toKeyCode 0x77 = KeyF8
toKeyCode 0x78 = KeyF9
toKeyCode 0x79 = KeyF10
toKeyCode 0x7a = KeyF11
toKeyCode 0x7b = KeyF12
toKeyCode 0x90 = KeyNumLock
toKeyCode 0x91 = KeyScrollLock
toKeyCode 0xa0 = KeyLeftShift
toKeyCode 0xa1 = KeyRightShift
toKeyCode 0xa2 = KeyLeftControl
toKeyCode 0xa3 = KeyRightControl
toKeyCode 0xa4 = KeyLeftAlt
toKeyCode 0xa5 = KeyRightAlt
toKeyCode 0xba = KeySemiColon
toKeyCode 0xbb = KeyEquals
toKeyCode 0xbc = KeyComma
toKeyCode 0xbd = KeyMinus
toKeyCode 0xbe = KeyPeriod
toKeyCode 0xbf = KeyForwardSlash
toKeyCode 0xc0 = KeyApostrophe
toKeyCode 0xdb = KeyLeftSquare
toKeyCode 0xdc = KeyBackSlash
toKeyCode 0xdd = KeyRightSquare
toKeyCode 0xde = KeyQuote
toKeyCode 0x30 = Key0
toKeyCode 0x31 = Key1
toKeyCode 0x32 = Key2
toKeyCode 0x33 = Key3
toKeyCode 0x34 = Key4
toKeyCode 0x35 = Key5
toKeyCode 0x36 = Key6
toKeyCode 0x37 = Key7
toKeyCode 0x38 = Key8
toKeyCode 0x39 = Key9
toKeyCode 0x41 = KeyA
toKeyCode 0x42 = KeyB
toKeyCode 0x43 = KeyC
toKeyCode 0x44 = KeyD
toKeyCode 0x45 = KeyE
toKeyCode 0x46 = KeyF
toKeyCode 0x47 = KeyG
toKeyCode 0x48 = KeyH
toKeyCode 0x49 = KeyI
toKeyCode 0x4a = KeyJ
toKeyCode 0x4b = KeyK
toKeyCode 0x4c = KeyL
toKeyCode 0x4d = KeyM
toKeyCode 0x4e = KeyN
toKeyCode 0x4f = KeyO
toKeyCode 0x50 = KeyP
toKeyCode 0x51 = KeyQ
toKeyCode 0x52 = KeyR
toKeyCode 0x53 = KeyS
toKeyCode 0x54 = KeyT
toKeyCode 0x55 = KeyU
toKeyCode 0x56 = KeyV
toKeyCode 0x57 = KeyW
toKeyCode 0x58 = KeyX
toKeyCode 0x59 = KeyY
toKeyCode 0x5a = KeyZ
toKeyCode _    = KeyNone

------------------------------------------------------------------------------
--  Input
------------------------------------------------------------------------------

data WindowMessage =
    WMMouseMove   PointInt
  | WMMouseButton PointInt Int Bool
  | WMSized       PointInt
  | WMKey         KeyCode Bool Bool
  | WMChar        Char
  | WMLeave

foreign import ccall safe "wrapper" makeInputSite :: (CInt -> CInt -> CInt -> CInt -> CInt -> IO ()) -> IO (FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> IO ()))
foreign import ccall safe mrtHandleInput :: PWindow -> FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> IO ()) -> IO ()

handleInput :: Window -> (WindowMessage -> IO ()) -> IO ()
handleInput window fio =
  let
    inputSite :: CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
    inputSite 0 x y _ _ = fio $ WMSized       (PointInt (fromIntegral x) (fromIntegral y))
    inputSite 1 x y _ _ = fio $ WMMouseMove   (PointInt (fromIntegral x) (fromIntegral y))
    inputSite 2 x y n b = fio $ WMMouseButton (PointInt (fromIntegral x) (fromIntegral y)) (fromIntegral n) (b /= 0)
    inputSite 3 x y z _ = fio $ WMLeave
    inputSite 4 a y n b = fio $ WMKey         (toKeyCode n) (b /= 0) (a /= 0)
    inputSite 5 x y n _ = fio $ WMChar        (chr $ fromIntegral n)
    inputSite _ _ _ _ _ = $error "bad message number"
  in do
    pis <- makeInputSite inputSite
    withForeignPtr window $ \pwindow ->
      mrtHandleInput pwindow pis

------------------------------------------------------------------------------
--  Window
------------------------------------------------------------------------------

foreign import ccall safe mrtOpenWindow       :: PPointInt -> PPointInt -> CWString -> IO PWindow
foreign import ccall safe mrtCloseWindow      :: PWindow -> IO ()
foreign import ccall safe mrtWindowClosed     :: PWindow -> IO CBool
foreign import ccall safe mrtGetDeviceContext :: PWindow -> IO PDeviceContext

openWindow       :: PointInt -> PointInt -> String -> IO Window
closeWindow      :: Window -> IO ()
windowClosed     :: Window -> IO Bool
getDeviceContext :: Window -> IO DeviceContext

openWindow       = apply3 mrtOpenWindow
closeWindow      = apply1 mrtCloseWindow
windowClosed     = apply1 mrtWindowClosed
getDeviceContext = apply1 mrtGetDeviceContext

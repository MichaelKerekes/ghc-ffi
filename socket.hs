------------------------------------------------------------------------------
--
--  Core.Socket
--
------------------------------------------------------------------------------

module Core.Socket (
  Socket,
  UDPSocket,
  SocketAddress,
  TCPListener,
  IPAddress,
  IPPort,
  saGetAddress,
  saGetPort,
  createSocketAddress,
  getSocketAddress,
  udpBind,
  udpRead,
  udpReadFrom,
  udpWrite,
  udpCreateUDPSocket,
  socketSetNoDelay,
  socketClose,
  socketRead,
  socketWrite,
  socketListen,
  socketCreateTCPListener,
  socketConnect,
  socketConnectAddress
) where

import Core
import Core.IO
import Core.FFI

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe

------------------------------------------------------------------------------
--  Types
------------------------------------------------------------------------------

data CSocketAddress
data CUDPSocket
data CSocket
data CTCPListener

type PSocketAddress = Ptr CSocketAddress
type PUDPSocket     = Ptr CUDPSocket
type PTCPListener   = Ptr CTCPListener
type PSocket        = Ptr CSocket

newtype SocketAddress = SocketAddress (ForeignPtr CSocketAddress)

type UDPSocket     = ForeignPtr CUDPSocket
type TCPListener   = ForeignPtr CTCPListener
type Socket        = ForeignPtr CSocket

------------------------------------------------------------------------------
--
--  SocketAddress
--
------------------------------------------------------------------------------

type CIPAddress = CUInt
type CIPPort    = CUShort

type IPAddress  = Word32
type IPPort     = Word16

instance ToC IPPort CIPPort where
  toC x f = f $ fromIntegral x

instance FromC CIPPort IPPort where
  fromC iox = map fromIntegral iox

------------------------------------------------------------------------------
--
--  SocketAddress
--
------------------------------------------------------------------------------

foreign import stdcall safe mrtGetAddress           :: PSocketAddress -> IO CIPAddress
foreign import stdcall safe mrtGetPort              :: PSocketAddress -> IO CIPPort
foreign import stdcall safe mrtCompareSocketAddress :: PSocketAddress -> PSocketAddress -> IO Int
foreign import stdcall safe mrtCreateSocketAddress  :: CIPAddress -> CIPPort -> IO PSocketAddress
foreign import stdcall safe mrtGetSocketAddress     :: CString -> CIPPort -> IO PSocketAddress

saGetAddress        :: SocketAddress -> IO IPAddress
saGetPort           :: SocketAddress -> IO IPPort
createSocketAddress :: IPAddress -> IPPort -> IO SocketAddress
getSocketAddress    :: String -> IPPort -> IO SocketAddress

saGetAddress (SocketAddress fpsa) = withForeignPtr fpsa $ \psa -> map fromIntegral $ mrtGetAddress psa
saGetPort    (SocketAddress fpsa) = withForeignPtr fpsa $ \psa -> map fromIntegral $ mrtGetPort    psa

-- !!!!! can unsafePerformIO be removed here?
-- !!!!! all the mrt calls are pure

instance Eq SocketAddress where
  (==) (SocketAddress fpsax) (SocketAddress fpsay) =
    unsafePerformIO $
      withForeignPtr fpsax $ \psax ->
        withForeignPtr fpsay $ \psay -> do
          order <- mrtCompareSocketAddress psax psay
          return $ order == 0

instance Ord SocketAddress where
  compare (SocketAddress fpsax) (SocketAddress fpsay) =
    unsafePerformIO $
      withForeignPtr fpsax $ \psax ->
        withForeignPtr fpsay $ \psay -> do
          order <- mrtCompareSocketAddress psax psay
          return $ if order < 0 then LT else if order > 0 then GT else EQ

instance ToDoc SocketAddress where
  doc sa =
    unsafePerformIO $ do
      address <- saGetAddress sa
      port    <- saGetPort    sa
      return $ $(printf "%:%") (hex address) port

createSocketAddress addr port = do
  psa  <- mrtCreateSocketAddress (fromIntegral addr) (fromIntegral port)
  fpsa <- reference psa
  return $ SocketAddress fpsa

getSocketAddress name port = do
  fpsa <- apply2 mrtGetSocketAddress name port
  return $ SocketAddress fpsa

------------------------------------------------------------------------------
--
--  UDPSocket
--
------------------------------------------------------------------------------

foreign import stdcall safe mrtUDPSocketClose    :: PUDPSocket -> IO ()
foreign import stdcall safe mrtUDPSocketBind     :: PUDPSocket -> CIPPort -> IO ()
foreign import stdcall safe mrtUDPSocketRead     :: PUDPSocket -> CString -> CInt -> IO CInt
foreign import stdcall safe mrtUDPSocketReadFrom :: PUDPSocket -> CString -> CInt -> Ptr PSocketAddress -> IO CInt
foreign import stdcall safe mrtUDPSocketWrite    :: PUDPSocket -> PSocketAddress -> CString -> CInt -> IO CInt
foreign import stdcall safe mrtUDPSocketCreate   :: IO PUDPSocket

udpClose           :: UDPSocket -> IO ()
udpBind            :: UDPSocket -> IPPort -> IO ()
udpRead            :: UDPSocket -> IO LBS.ByteString
udpReadFrom        :: UDPSocket -> IO (LBS.ByteString, SocketAddress)
udpWrite           :: UDPSocket -> SocketAddress -> LBS.ByteString -> IO ()
udpCreateUDPSocket :: IO UDPSocket

udpClose           = apply1 mrtUDPSocketClose
udpBind            = apply2 mrtUDPSocketBind
udpCreateUDPSocket = apply0 mrtUDPSocketCreate

udpPacketSize = 512

udpRead udpPort =
  withForeignPtr udpPort $ \pudpPort ->
    allocaBytes udpPacketSize $ \ptr -> do
      ccount <- mrtUDPSocketRead pudpPort ptr (fromIntegral udpPacketSize)
      let count = fromIntegral ccount
      bs <- BS.packCStringLen (ptr, count)
      return $ LBS.fromChunks [bs]

udpReadFrom udpSocket  =
  withForeignPtr udpSocket $ \pudpSocket ->
    allocaBytes udpPacketSize $ \ptr ->
      alloca $ \ppsa -> do
        ccount <- mrtUDPSocketReadFrom pudpSocket ptr (fromIntegral udpPacketSize) ppsa
        let count = fromIntegral ccount
        psa  <- peek ppsa
        fpsa <- reference psa
        bs   <- BS.packCStringLen (ptr, count)
        return (LBS.fromChunks [bs], SocketAddress fpsa)

udpWrite udpSocket (SocketAddress fpsa) lbs = do
  withForeignPtr udpSocket $ \pudpSocket ->
    withForeignPtr fpsa $ \psa ->
      BS.useAsCStringLen (BS.concat $ LBS.toChunks lbs) $ \(ptr, count) -> do
        ccount <- mrtUDPSocketWrite pudpSocket psa ptr (fromIntegral count)
        let count = fromIntegral ccount
        when (count /= LBS.length lbs) $fail
        return ()

------------------------------------------------------------------------------
--
--  TCPSocket
--
------------------------------------------------------------------------------

foreign import stdcall safe mrtSetNoDelay           :: PSocket -> Bool -> IO ()
foreign import stdcall safe mrtClose                :: PSocket -> IO ()
foreign import stdcall safe mrtRead                 :: PSocket -> CString -> CInt -> IO CInt
foreign import stdcall safe mrtWrite                :: PSocket -> CString -> CInt -> IO ()
foreign import stdcall safe mrtConnect              :: CString -> CIPPort -> IO PSocket
foreign import stdcall safe mrtConnectSocketAddress :: CIPAddress -> CIPPort -> IO PSocket

socketSetNoDelay     :: Socket    -> Bool           -> IO ()
socketClose          :: Socket                      -> IO ()
socketRead           :: Socket                      -> IO LBS.ByteString
socketWrite          :: Socket    -> LBS.ByteString -> IO ()
socketConnect        :: String    -> IPPort         -> IO (Maybe Socket)
socketConnectAddress :: IPAddress -> IPPort         -> IO (Maybe Socket)

socketSetNoDelay = apply2 mrtSetNoDelay
socketClose      = apply1 mrtClose

socketConnect name port =
  withCString name $ \pname ->
    toC port $ \cport -> do
      psocket <- mrtConnect pname cport
      if (psocket == nullPtr) then
        return None
      else do
        socket <- reference psocket
        return $ Some socket

socketConnectAddress addr port = do
  psocket <- mrtConnectSocketAddress (fromIntegral addr) (fromIntegral port)
  if (psocket == nullPtr) then
    return None
  else do
    socket <- reference psocket
    return $ Some socket

tcpPacketSize = 64 * 1024

socketRead socket =
  let
    readChunks = unsafeInterleaveIO $ do
      (bs, count) <-
        withForeignPtr socket $ \psocket ->
          allocaBytes tcpPacketSize $ \ptr -> do
            ccount <- mrtRead psocket ptr (fromIntegral tcpPacketSize)
            let count = fromIntegral ccount
            bs <- BS.packCStringLen (ptr, count)
            return (bs, count)
      if count == 0 then
        return []
      else do
        bss <- readChunks
        return $ bs : bss
  in do
    bss <- readChunks
    return $ LBS.fromChunks bss

socketWrite socket lbs = do
  withForeignPtr socket $ \psocket ->
    BS.useAsCStringLen (BS.concat $ LBS.toChunks lbs) $ \(ptr, count) -> do
      mrtWrite psocket ptr (fromIntegral count)

------------------------------------------------------------------------------
--
--  TCPListener
--
------------------------------------------------------------------------------

foreign import stdcall safe mrtListen            :: PTCPListener -> IO PSocket
foreign import stdcall safe mrtCreateTCPListener :: CIPPort -> IO PTCPListener

socketCreateTCPListener :: IPPort -> IO TCPListener
socketCreateTCPListener = apply1 mrtCreateTCPListener

socketListen :: TCPListener -> IO Socket
socketListen tcpListener =
  withForeignPtr tcpListener $ \ptcpListener -> do
    psocket <- mrtListen ptcpListener
    when (psocket == nullPtr) $fail
    reference psocket

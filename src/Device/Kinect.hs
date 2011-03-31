{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

-- | This Kinect module is a lightweight wrapper for the libfreenect C library.
-- The majority of its usage remains the same as the C code, although it wraps
-- pointer management into a more Haskell style API.
module Device.Kinect
    ( -- * Primary Data Types
      Context
    , Device
    
      -- * C Enums
    , LogLevel(..)
    , LedOption(..)
    , TiltStatus(..)
    
      -- * Data Format Types
    , VideoFormat(..)
    , DepthFormat(..)
    
      -- * C Function Types
    , LogCallback
    , DataCallback
    
      -- * Constant Values
    , frameWidth
    , frameHeight
    , pixelCount
    
      -- * Context Functions
    , initialize
    , shutdown
    , setLogLevel
    , onLog
    , processEvents
    , deviceCount
    
      -- * Device Functions
    , openDevice
    , closeDevice
    , setLed
    , onVideo
    , onDepth
    , setVideoFormat
    , setDepthFormat
    , startVideo
    , startDepth
    , stopVideo
    , stopDepth
    , tiltStatus
    , tiltAngle
    , setTiltAngle
    ) where

import Data.Word (Word32)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble, CInt, CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (Storable, alignment, peek, peekElemOff, sizeOf)
import Helpers.Composition ((.>), (.<))
import Helpers.IO (tryC, tryIO)

-- | The amount of data to output when logging, in ascending order.
data LogLevel = Fatal
              | Error
              | Warning
              | Notice
              | Info
              | Debug
              | Spew
              | Flood
              deriving (Eq, Show)

-- | The options for the Kinect LED light.
data LedOption = Off
               | Green
               | Yellow
               | Red
               | Blink
               | Panic
               deriving (Eq, Show)

-- | The potential values of the tilt motor status.
data TiltStatus = Stopped
                | Limit
                | Moving
                deriving (Eq, Show)

-- | The formats available from the Kinect video input.
data VideoFormat = RGB
                 | Bayer
                 | IR8Bit
                 | IR10Bit
                 | IR10BitPacked
                 | YuvRGB
                 | YuvRaw
                 deriving (Eq, Show)

-- | The formats available form the Kinect depth input.
data DepthFormat = D11Bit
                 | D10Bit
                 | D11BitPacked
                 | D10BitPacked
                 deriving (Eq, Show)


-- | A function for use as a callback when logging.
type LogCallback = Context -> LogLevel -> String -> IO ()
-- | The actual type of the underlying C callback for logging.
type LogFunction = Context -> CInt -> CString -> IO ()

-- | A function for use as a callback when receiving data.
type DataCallback a = Device -> [a] -> Word32 -> IO ()
-- | The actual type of the underlying C callback for data.
type DataFunction a = Device -> Ptr a -> CUInt -> IO ()

-- | 
type LogSetter = Context -> FunPtr LogFunction -> IO CInt
-- | 
type DataSetter a = Device -> FunPtr (DataFunction a) -> IO ()


data Cxt
instance Storable Cxt where
    sizeOf _    = 40
    alignment _ = 4
type Context = Ptr Cxt

data Dev
instance Storable Dev where
    sizeOf _    = 408
    alignment _ = 4
type Device = Ptr Dev

foreign import ccall "wrapper"
    logWrap :: LogFunction -> IO (FunPtr LogFunction)
foreign import ccall "wrapper"
    dataWrap :: DataFunction a -> IO (FunPtr (DataFunction a))

foreign import ccall freenect_init :: Ptr Context -> Ptr () -> IO CInt
foreign import ccall freenect_shutdown :: Context -> IO CInt
foreign import ccall freenect_open_device :: Context -> Ptr Device -> CInt -> IO CInt
foreign import ccall freenect_close_device :: Device -> IO CInt
foreign import ccall freenect_set_log_level :: Context -> CInt -> IO CInt
foreign import ccall freenect_set_log_callback :: LogSetter
foreign import ccall freenect_process_events :: Context -> IO CInt
foreign import ccall freenect_num_devices :: Context -> IO CInt
foreign import ccall freenect_set_led :: Device -> CInt -> IO CInt
foreign import ccall freenect_set_video_format :: Device -> CInt -> IO CInt
foreign import ccall freenect_set_depth_format :: Device -> CInt -> IO CInt
foreign import ccall freenect_set_video_callback :: DataSetter a
foreign import ccall freenect_set_depth_callback :: DataSetter a
foreign import ccall freenect_start_video :: Device -> IO CInt
foreign import ccall freenect_start_depth :: Device -> IO CInt
foreign import ccall freenect_stop_video :: Device -> IO CInt
foreign import ccall freenect_stop_depth :: Device -> IO CInt
foreign import ccall freenect_update_tilt_state :: Device -> IO CInt
-- foreign import ccall freenect_get_tilt_status :: Device -> IO CInt
foreign import ccall freenect_get_tilt_degs :: Device -> IO CDouble
foreign import ccall freenect_set_tilt_degs :: Device -> CDouble -> IO CInt


-- | The width of freenect frame data.
frameWidth :: Int
frameWidth = 640

-- | The height of freenect frame data.
frameHeight :: Int
frameHeight = 480

-- | The number of pixels in freenect frame data.
pixelCount :: Int
pixelCount = frameWidth * frameHeight


initialize :: IO Context
initialize = alloca $ \addr -> do
    tryC "Freenect failed to initialize" (freenect_init addr nullPtr)
    peek addr

shutdown :: Context  -- ^ 
         -> IO ()
shutdown context =
    tryC "Freenect failed to shutdown" (freenect_shutdown context)

openDevice :: Context    -- ^
           -> Int        -- ^
           -> IO Device
openDevice context index = alloca $ \addr -> do
    tryC "Device failed to open"
        (freenect_open_device context addr (fromIntegral index))
    peek addr

closeDevice :: Device   -- ^ 
            -> IO ()
closeDevice device =
    tryC "Device failed to close" (freenect_close_device device)

setLogLevel :: Context   -- ^ 
            -> LogLevel  -- ^
            -> IO ()
setLogLevel context level =
    freenect_set_log_level context (levelToInt level) >> return ()

-- | Internal reference to the log callback, so we don't have to worry about
-- converting the function back and forth if the user really wants to get it
-- back out. Initially set to a useless function.
-- lcb :: IORef LogCallback
-- lcb = unsafePerformIO (newIORef $ \_ _ _ -> return ())
-- 
-- logCallback :: Context         -- ^ 
--             -> IO LogCallback
-- logCallback _ = readIORef lcb

onLog :: Context      -- ^ 
      -> LogCallback  -- ^ 
      -> IO ()
onLog context callback = logWrap ccallback >>=
    freenect_set_log_callback context >> return ()
  where
    ccallback cxt level message = do
        actual <- intToIOLevel level
        peekCString message >>= callback cxt actual

deviceCount :: Context  -- ^
            -> IO Int
deviceCount = tryIO "Failed to read device count"
    (>= 0) fromIntegral . freenect_num_devices

processEvents :: Context  -- ^
              -> IO ()
processEvents = tryC "Failed to process events" . freenect_process_events

setLed :: Device     -- ^
       -> LedOption  -- ^
       -> IO ()
setLed device led = freenect_set_led device (ledToInt led) >> return ()

onVideo :: Integral a      -- ^ 
        => Storable a      -- ^ 
        => Device          -- ^ 
        -> DataCallback a  -- ^ 
        -> IO ()
onVideo = onData freenect_set_video_callback

onDepth :: Integral a      -- ^ 
        => Storable a      -- ^ 
        => Device          -- ^ 
        -> DataCallback a  -- ^ 
        -> IO ()
onDepth = onData freenect_set_depth_callback

onData :: Integral a       -- ^ 
       => Storable a       -- ^ 
       => DataSetter a     -- ^ 
       -> Device           -- ^ 
       -> DataCallback a   -- ^ 
       -> IO ()
onData function device callback = dataWrap wrapped >>= function device
  where
    wrapped dev ref timestamp = do
        list <- peekArray pixelCount ref
        callback dev list (fromIntegral timestamp)

setVideoFormat :: Device       -- ^ 
               -> VideoFormat  -- ^ 
               -> IO ()
setVideoFormat = tryC "Failed to set video format" .<
    freenect_set_video_format .> videoFormatToInt

setDepthFormat :: Device       -- ^ 
               -> DepthFormat  -- ^ 
               -> IO ()
setDepthFormat = tryC "Failed to set depth format" .<
    freenect_set_depth_format .> depthFormatToInt

startVideo :: Device  -- ^ 
           -> IO ()
startVideo = tryC "Failed to start video" . freenect_start_video

startDepth :: Device  -- ^ 
           -> IO ()
startDepth = tryC "Failed to start depth" . freenect_start_depth

stopVideo :: Device  -- ^ 
          -> IO ()
stopVideo = tryC "Failed to stop video" . freenect_stop_video

stopDepth :: Device  -- ^ 
          -> IO ()
stopDepth = tryC "Failed to stop depth" . freenect_stop_depth

updateTiltState :: Device   -- ^ 
                -> IO CInt
updateTiltState = tryIO "Failed to update tilt state."
    (>= 0) id . freenect_update_tilt_state

tiltStatus :: Device         -- ^ 
           -> IO TiltStatus
tiltStatus _ = return Stopped
-- updateTiltState device >> freenect_get_tilt_status device >>= intToIOTiltStatus

tiltAngle :: Device     -- ^ 
          -> IO Double
tiltAngle device =
    updateTiltState device >> freenect_get_tilt_degs device >>= return . realToFrac

setTiltAngle :: Device  -- ^ 
             -> Double  -- ^ 
             -> IO ()
setTiltAngle = tryC "Failed to set device tilt" .<
    freenect_set_tilt_degs .> realToFrac

-- | Transforms a 'LogLevel' into its corresponding 'Integral' representation.
levelToInt :: Integral a  -- ^ Don't constrain the type to a particular int
           => LogLevel    -- ^ The log level to convert
           -> a
levelToInt Fatal   = 0
levelToInt Error   = 1
levelToInt Warning = 2
levelToInt Notice  = 3
levelToInt Info    = 4
levelToInt Debug   = 5
levelToInt Spew    = 6
levelToInt Flood   = 7

intToIOLevel :: Integral a  -- ^ 
             => a           -- ^ 
             -> IO LogLevel
intToIOLevel 0 = return Fatal
intToIOLevel 1 = return Error
intToIOLevel 2 = return Warning
intToIOLevel 3 = return Notice
intToIOLevel 4 = return Info
intToIOLevel 5 = return Debug
intToIOLevel 6 = return Spew
intToIOLevel 7 = return Flood
intToIOLevel _ = ioError (userError "Unknown log level encountered")

ledToInt :: Integral a  -- ^ 
         => LedOption   -- ^ 
         -> a
ledToInt Off    = 0
ledToInt Green  = 1
ledToInt Red    = 2
ledToInt Yellow = 3
ledToInt Blink  = 4
ledToInt Panic  = 6

videoFormatToInt :: Integral a   -- ^ 
                 => VideoFormat  -- ^ 
                 -> a
videoFormatToInt RGB           = 0
videoFormatToInt Bayer         = 1
videoFormatToInt IR8Bit        = 2
videoFormatToInt IR10Bit       = 3
videoFormatToInt IR10BitPacked = 4
videoFormatToInt YuvRGB        = 5
videoFormatToInt YuvRaw        = 6

depthFormatToInt :: Integral a   -- ^ 
                 => DepthFormat  -- ^ 
                 -> a
depthFormatToInt D11Bit       = 0
depthFormatToInt D10Bit       = 1
depthFormatToInt D11BitPacked = 2
depthFormatToInt D10BitPacked = 3

-- intToIOTiltStatus :: Integral a     -- ^ 
--                   => a              -- ^ 
--                   -> IO TiltStatus
-- intToIOTiltStatus 0 = return Stopped
-- intToIOTiltStatus 1 = return Limit
-- intToIOTiltStatus 4 = return Moving
-- intToIOTiltStatus _ = ioError (userError "Failed to read tilt status")

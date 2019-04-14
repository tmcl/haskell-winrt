module System.Windows.WinRT.RoInit (roInitialize, RoInitType (SingleThreaded, MultiThreaded))
where

import System.Win32.Types (HRESULT)
import System.Windows.WinRT.Monad
import Control.Monad.IO.Class (liftIO)

foreign import ccall "RoInitialize"
    c_RoInitialize ::  RO_INIT_TYPE → IO HRESULT

newtype RO_INIT_TYPE = RO_INIT_TYPE Int
rO_INIT_SINGLETHREADED = RO_INIT_TYPE 0
rO_INIT_MULTITHREADED = RO_INIT_TYPE 1

data RoInitType = SingleThreaded | MultiThreaded

toRO_INIT_TYPE :: RoInitType → RO_INIT_TYPE
toRO_INIT_TYPE SingleThreaded = rO_INIT_SINGLETHREADED
toRO_INIT_TYPE MultiThreaded = rO_INIT_MULTITHREADED

roInitialize :: RoInitType → WinRT ()
roInitialize threadedness = do
   res ← liftIO $ c_RoInitialize (toRO_INIT_TYPE threadedness)
   throwHResult (res, ())

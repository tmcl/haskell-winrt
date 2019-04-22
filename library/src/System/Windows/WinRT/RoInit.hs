{-# LANGUAGE TypeFamilies #-}

module System.Windows.WinRT.RoInit (
    roInitialize, 
    RoInitType (SingleThreaded, MultiThreaded),
    getActivationFactory,
    ActivatableFactory(..),
    ClassId(..)
    )
where

import Foreign
import Foreign.C.Types
import System.Windows.WinRT.Monad
import System.Windows.WinRT.IUnknown
import System.Windows.WinRT.Lowlevel.Info
import System.Windows.WinRT.Lowlevel.HSTRING
import System.Windows.GUID
import Control.Monad.IO.Class (liftIO)

foreign import ccall "RoGetActivationFactory"
    c_RoGetActivationFactory ::  HSTRING → Ptr (IID b) → Ptr (Ptr (Ptr b)) → IO HRESULT



-- class Storable vtable => RuntimeInterface vtable where
--     data CInstance vtable = CInstance (Ptr vtable)
--     data Instance vtable = Instance (CInstance vtable) vtable
--     iid :: IID vtable
--     classId :: ClassId vtable

newtype ClassId a = ClassId String

class Storable b => ActivatableFactory b where
    af_iid :: IID b

getActivationFactory :: ActivatableFactory b => ClassId a → WinRT (RuntimeInstance b)
getActivationFactory (ClassId classId) =
    withHString (pack classId) $ \h_classId →
        passthrough2 alloca2 $ \p_iid pp_applicationStatics -> do
            liftIO $ poke p_iid af_iid
            try $ c_RoGetActivationFactory h_classId p_iid pp_applicationStatics
            liftIO $ do
                p_applicationStatics ← peek pp_applicationStatics
                applicationStatics ← peek p_applicationStatics
                vtable ← peek applicationStatics
                foreignPtr ← newForeignPtr_ p_applicationStatics
                return RuntimeInstance {..}

foreign import ccall "RoInitialize"
    c_RoInitialize ::  RO_INIT_TYPE → IO HRESULT

newtype RO_INIT_TYPE = RO_INIT_TYPE Int
rO_INIT_SINGLETHREADED :: RO_INIT_TYPE
rO_INIT_SINGLETHREADED = RO_INIT_TYPE 0
rO_INIT_MULTITHREADED :: RO_INIT_TYPE
rO_INIT_MULTITHREADED = RO_INIT_TYPE 1

data RoInitType = SingleThreaded | MultiThreaded

toRO_INIT_TYPE :: RoInitType → RO_INIT_TYPE
toRO_INIT_TYPE SingleThreaded = rO_INIT_SINGLETHREADED
toRO_INIT_TYPE MultiThreaded = rO_INIT_MULTITHREADED

roInitialize :: RoInitType → WinRT ()
roInitialize threadedness = do
   res ← liftIO $ c_RoInitialize (toRO_INIT_TYPE threadedness)
   throwHResult (res, ())

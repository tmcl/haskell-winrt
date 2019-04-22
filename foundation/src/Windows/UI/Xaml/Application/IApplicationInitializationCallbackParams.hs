{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams where

import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Inspectable

interfaceName :: String
interfaceName = "Windows.UI.Xaml.IApplicationInitializationCallbackParams"


iid :: IID IApplicationInitializationCallbackParamsVtbl 
iid = IID $ GUID 0x751B792E 0x5772 0x4488 
    0x8B 0x87 0xF5 0x47 0xFA 0xA6 0x44 0x74

newtype IApplicationInitializationCallbackParamsVtbl = IApplicationInitializationCallbackParamsVtbl {
   inspectable :: IInspectableVtbl
}
  deriving (Storable)

type IApplicationInitializationCallbackParams = Ptr IApplicationInitializationCallbackParamsVtbl

data ApplicationInitializationCallbackParams = ApplicationInitializationCallbackParams {
   foreignPtr :: ForeignPtr IApplicationInitializationCallbackParams,
   vtable :: IApplicationInitializationCallbackParamsVtbl
}

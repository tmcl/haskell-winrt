module Windows.UI.Xaml.IApplicationInitializationCallback where

import Foreign
import System.Windows.GUID
import System.Windows.WinRT.HString
import System.Windows.WinRT.Inspectable
import Windows.UI.Xaml.IApplicationInitializationCallbackParams

-- interfaceName :: String
-- interfaceName = "Windows.UI.Xaml.IApplicationInitializationCallback"


iid :: IID IApplicationInitializationCallbackVtbl 
iid = IID $ GUID 0xB6351C55 0xC284 0x46E4
   0x83 0x10 0xfB 0x09 0x67 0xFA 0xB7 0x6F

data IApplicationInitializationCallbackVtbl = IApplicationInitializationCallbackVtbl {
   iunknown :: IUnknownVtbl,
   cfp_Invoke :: FunPtr (Ptr IApplicationInitializationCallbackParams → HRESULT)
}
  deriving (Storable)

type IApplicationInitializationCallback = Ptr IApplicationInitializationCallbackVtbl

data ApplicationInitializationCallback = ApplicationInitializationCallback {
   foreignPtr :: ForeignPtr IApplicationInitializationCallback,
   vtable :: IApplicationInitializationCallbackVtbl
}

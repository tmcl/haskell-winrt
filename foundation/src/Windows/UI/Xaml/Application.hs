module Windows.UI.Xaml.Application where

import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.RoInit

classId :: ClassId IApplicationVtbl
classId = ClassId "Windows.UI.Xaml.Application"

iid :: IID IApplicationVtbl
iid = IID $ GUID 0x74B861A1
                      0x7487
                      0x46A9
                      0x9A 0x6E 
                      0xC7 0x8B 0x51 0x27 0x26 0xC5

data IApplicationVtbl = IApplicationVtbl {
   iapp_inspectable :: IInspectableVtbl,
   cfp_get_Resources :: FunPtr (Ptr (Ptr IResourceDictionary) → HRESULT),
   cfp_put_Resources :: FunPtr (Ptr IResourceDictionary → HRESULT)
}

type IApplication = Ptr IApplicationVtbl

data IResourceDictionary
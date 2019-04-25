module Windows.UI.Xaml.Application where

import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.IUnknown
import System.Windows.WinRT.RoInit
import System.Windows.WinRT.Lowlevel.Info

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

instance IsUnknown IApplicationVtbl where
   fp_QueryInterface = fp_QueryInterface . iapp_inspectable

instance Storable IApplicationVtbl where
   sizeOf it = sizeOf (iapp_inspectable it) + 2 * sizeOfMethodPtr
   alignment it = alignment (cfp_get_Resources it)
   peek ptr = do
      iapp_inspectable ← peek (ptr `plusPtr` 0)
      cfp_get_Resources ← peek (ptr `plusPtr` sizeOf iapp_inspectable)
      cfp_put_Resources ← peek (ptr `plusPtr` (sizeOf iapp_inspectable + sizeOfMethodPtr))
      return IApplicationVtbl {..}
   poke ptr IApplicationVtbl {..} = do
      poke (ptr `plusPtr` 0) iapp_inspectable
      poke (ptr `plusPtr` sizeOf iapp_inspectable) cfp_get_Resources
      poke (ptr `plusPtr` (sizeOf iapp_inspectable + sizeOfMethodPtr)) cfp_get_Resources



type IApplication = Ptr IApplicationVtbl
type Application = RuntimeInstance IApplicationVtbl

data IResourceDictionary
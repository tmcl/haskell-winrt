module System.Windows.WinRT.Lowlevel.HSTRING
where

import Foreign hiding (void)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import System.Windows.WinRT.Lowlevel.Monad
import Control.Monad

newtype HString = HString (ForeignPtr HSTRING__)

withHString :: HString → (HSTRING → WinRT a) → WinRT a
withHString (HString str) = passthrough $ withForeignPtr str

data HSTRING__
type HSTRING = Ptr HSTRING__

foreign import ccall "WindowsCreateString"
    c_WindowsCreateString :: CWString → CUInt → Ptr HSTRING → IO HRESULT

foreign import ccall "&WindowsDeleteString"
    cfp_WindowsDeleteString :: FunPtr (HSTRING → IO ()) -- actually, HRESULT, but we are compelled to ignore the error

-- todo: implement fastpass HSTRINGs
pack :: String → HString
pack str = unsafePerformIO . alloca $ \p_hstring →
      withCWStringLen str $ \(c_str, len) → do
         void $ c_WindowsCreateString c_str (fromIntegral len) p_hstring
         c_hstring ←peek p_hstring
         HString <$> newForeignPtr cfp_WindowsDeleteString c_hstring
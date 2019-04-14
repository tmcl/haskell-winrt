module System.Windows.WinRT (
   run,
   module System.Windows.WinRT.Monad, 
   HRESULT
) where

import Control.Monad.Except
import System.Windows.WinRT.Monad
import System.Windows.WinRT.RoInit
import System.Win32.Types (HRESULT)

run :: WinRT a → IO (Either HRESULT a)
run it = runExceptT . unWinRT $ roInitialize MultiThreaded >> it
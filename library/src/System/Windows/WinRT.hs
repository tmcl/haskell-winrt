module System.Windows.WinRT (
   run,
   module System.Windows.WinRT.Monad,
   module System.Windows.WinRT.HString,
   module System.Windows.WinRT.Inspectable
) where

import Control.Monad.Except
import System.Windows.WinRT.HString
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.Monad
import System.Windows.WinRT.RoInit

run :: WinRT a → IO (Either HRESULT a)
run it = runExceptT . unWinRT $ roInitialize MultiThreaded >> it
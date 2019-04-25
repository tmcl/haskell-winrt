{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Prelude hiding (print, putStrLn, putStr)
import System.Windows.WinRT
import System.Windows.WinRT.RoInit
import Control.Monad.IO.Class
import qualified Windows.UI.Xaml.Application
import Windows.UI.Xaml.Application.IApplicationStatics
import Windows.UI.Xaml.Application.IApplicationFactory
import Windows.UI.Xaml.Application.IApplicationInitializationCallback
import Windows.UI.Xaml.Application.IApplicationOverrides_impl
import Graphics.Win32.Misc
import Debug.Trace (traceIO)
import Control.Monad

traceIOS :: (MonadIO m, Show a) => a → m ()
traceIOS = liftIO . traceIO . show


main :: IO ()
main = do
   traceIO " i do beginneth"
   res <- run it
   traceIOS res
   _ ← messageBox Nothing "all done" "i quit now" mB_OK
   return ()

altInvoker :: () → WinRT ()
altInvoker _ = do
   traceIOS "i am the start of the alt invoker"

   factory :: ApplicationFactory ← getActivationFactory Windows.UI.Xaml.Application.classId
   traceIOS "got the factory, yes I did"
   let overrides = def {
      m_OnLaunched = Just $ \_ → do
         traceIOS "the application is being launched" 
         return 0
   }
   void $ createApplication factory overrides

   traceIOS "done the thingamie"

it :: WinRT ()
it = do
   traceIOS "initialised"
   statics :: ApplicationStatics ← getActivationFactory Windows.UI.Xaml.Application.classId
   traceIOS "got the statics, yes i did"
   startApplication statics $ ApplicationInitializationCallbackImpl altInvoker
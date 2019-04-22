{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main(main, ex_main) where

import Prelude hiding (print, putStrLn, putStr)
import Foreign hiding (void)
import Foreign.C
import System.Windows.WinRT
import System.Windows.WinRT.RoInit
import System.Windows.WinRT.IUnknown
import Control.Monad.IO.Class
import qualified Windows.UI.Xaml.Application
import Windows.UI.Xaml.Application.IApplicationStatics
import Windows.UI.Xaml.Application.IApplicationInitializationCallback
-- import Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams
import Data.IORef
-- import Control.Concurrent
import Graphics.Win32.Misc
import Control.Monad
import Debug.Trace (traceIO)

-- traceIO :: String → IO ()
-- traceIO _ = return ()

traceIOS :: (MonadIO m, Show a) => a → m ()
traceIOS = liftIO . traceIO . show

-- foreign import ccall "query_interface"
--    test_c_query_interface :: Ptr () → Ptr () → Ptr (Ptr ()) -> IO HRESULT
-- 
-- foreign import ccall "&query_interface"
--    ptest_c_query_interface :: FunPtr (Ptr () → Ptr () → Ptr (Ptr ()) -> IO HRESULT)

foreign import ccall "&add_ref"
   ptest_c_add_ref :: FunPtr (Ptr () -> IO HRESULT)

foreign import ccall "&release"
   ptest_c_release :: FunPtr (Ptr () -> IO HRESULT)

-- foreign import ccall "&invoke"
--    ptest_c_invoke :: FunPtr(Ptr () → Ptr () -> IO HRESULT)

-- foreign import ccall "dynamic"
--    mkInvoke' :: FunPtr(Ptr () → Ptr () -> IO HRESULT) -> Ptr () → Ptr () -> IO HRESULT

foreign import ccall "dynamic"
   mkgetiids :: FunPtr GetIidsType -> GetIidsType

foreign export ccall ex_main :: IO ()

ex_main :: IO ()
ex_main = main

main :: IO ()
main = do
   traceIO " i do beginneth"
   res <- run it
   traceIOS res
   _ ← messageBox Nothing "all done" "i quit now" mB_OK
   return ()

myLocalInfo :: IO (MyUnknown MyLocalInfo)
myLocalInfo = newIORef $ MyUnknown 0 MyLocalInfo

invokera :: Invoker
invokera _ = messageBox Nothing "i am invoker a" "invoker a" mB_OK >> return 0

invoker :: Invoker
invoker _ = messageBox Nothing "i am tother" "tother" mB_OK >> return 0

foreign import ccall "wrapper"
   mkQIT :: UnsafeQueryInterfaceType → IO (FunPtr UnsafeQueryInterfaceType)

qit :: Ptr IUnknown → REFIID → Ptr (Ptr ()) → IO HRESULT
qit ptr _ p_ptr = do
     void $ messageBox Nothing "thou hast queried the interface type" "qit" mB_OK 
     poke p_ptr (castPtr ptr)
     return 0

showmsg :: (MonadIO m) => String → m ()
showmsg msg = void $ liftIO $ messageBox Nothing msg "a body" mB_OK

-- printmsg :: (MonadIO m, Show a) => a → m ()
-- printmsg = showmsg . show 

foreign import ccall "wrapper"
      mkInvoke :: Invoker → IO (FunPtr Invoker)

initVtbl :: IO IApplicationInitializationCallbackVtbl
initVtbl = do
   iunknown ← my_vtable
   cfp_Invoke ← mkInvoke invoker
   return IApplicationInitializationCallbackVtbl {..}

mkOnInit :: IO Something
mkOnInit = Something <$> initVtbl <*> myLocalInfo 

data Whatever = Whatever 
instance Storable Whatever where
   sizeOf _ = 6 * sizeOf ptest_c_add_ref
   alignment _ = alignment ptest_c_add_ref
   peek _ptr = return Whatever
   poke ptr _ = do
      let n = sizeOf ptest_c_add_ref
      p_invoke ← mkInvoke invokera
      p_queryInterface ← mkQIT qit
      poke (ptr `plusPtr` 0) (ptr `plusPtr` (2*n))
      poke (ptr `plusPtr` (1*n)) nullPtr
      poke (ptr `plusPtr` (2*n)) p_queryInterface
      poke (ptr `plusPtr` (3*n)) ptest_c_add_ref
      poke (ptr `plusPtr` (4*n)) ptest_c_release
      poke (ptr `plusPtr` (5*n)) p_invoke


it :: WinRT ()
it = do
   traceIOS "initialised"
      --threadDelay 1000000
   -- liftIO $ threadDelay 19666666
   statics :: ApplicationStatics ← getActivationFactory Windows.UI.Xaml.Application.classId
   traceIOS "got the statics, yes i did"
   withRuntimeInstance statics $ \statics_vtbl statics_this → do
      let start = mkStart $ cfp_Start statics_vtbl
      liftIO $ alloca $ \pp_iids -> 
         alloca $ \p_len -> do
            let i = inspectable statics_vtbl
            hres <- (mkgetiids $ cfp_GetIids i) (castPtr statics_this) p_len pp_iids
            traceIOS hres
            len <- peek p_len
            p_iids <- peek pp_iids
            forM_ [0..fromIntegral len] $ 
               traceIOS <=< peekElemOff p_iids 

      liftIO $ alloca $ \p_whatever → do
            showmsg "in the second"
            traceIOS "yes okay"
            poke p_whatever Whatever
            traceIO "poked"
            res ← start statics_this (castPtr p_whatever)
            traceIO $ show res
            traceIOS "ran - to be shown"
                
      liftIO $ alloca $ \p_something → do
         showmsg "in the third"
         traceIO "allocaed the initialiser"
         onInit ← mkOnInit
         poke p_something onInit
         traceIO "poked the initialiser"
         -- _ <- start statics_this nullPtr
         traceIO "null ptr - skipped"
         res ← start statics_this (castPtr p_something)
         traceIO "—ran— the initialiser"
         traceIOS res




   -- insp ← newInspectable $ pack "Windows.UI.Xaml.Application"
   -- app ← queryInterface 
   -- liftIO $ putStrLn "created"
   




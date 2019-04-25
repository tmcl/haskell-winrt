{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Windows.WinRT.Lowlevel.Monad (
   HRESULT,
   WinRT,
   unWinRT,
   try,
   wrap,
   unwrap,
   throwHResult,
   passthrough,
   passthrough2,
   passthrough3,
   throwHResult',
   returnHresult,
   CULong(..),
   e_no_interface,
   e_outofmemory
) where

import Control.Monad.Except
import Foreign.C.Types

e_no_interface :: HRESULT
e_no_interface = 0x80004002

e_outofmemory :: HRESULT
e_outofmemory =  0x8007000E

type HRESULT = CULong
type WinRTCore a = ExceptT HRESULT IO a
type WinRTUnwrapped a = IO (Either HRESULT a)

newtype WinRT a = WinRT { unWinRT :: WinRTCore a}
   deriving (Functor, Applicative, Monad, MonadIO, MonadError HRESULT, MonadFix)

passthrough :: ((a → WinRTUnwrapped b) → WinRTUnwrapped b) → (a → WinRT b) → WinRT b
passthrough f g = wrap $ f (unwrap . g)

passthrough2 :: ((a → b → WinRTUnwrapped c) → WinRTUnwrapped c) → (a → b → WinRT c) → WinRT c
passthrough2 f g = wrap $ f (\a b → unwrap $ g a b)

passthrough3 :: 
   ((a → b → c → WinRTUnwrapped z) → WinRTUnwrapped z) 
   → (a → b → c → WinRT z) 
   → WinRT z
passthrough3 f g = wrap $ f (\a b c → unwrap $ g a b c)

wrap :: WinRTUnwrapped a → WinRT a
wrap = WinRT . ExceptT

unwrap :: WinRT a → WinRTUnwrapped a
unwrap = runExceptT . unWinRT

returnHresult :: WinRT () → IO HRESULT
returnHresult it = do
   res ← unwrap it
   return $ case res of
      Left hres → hres
      Right () → 0

try :: IO HRESULT → WinRT ()
try it = liftIO it >>= flip throwHResult' () 

throwHResult' ∷ HRESULT → a → WinRT a
throwHResult' = curry throwHResult

throwHResult ∷ (HRESULT, a) → WinRT a
throwHResult (0, a)  = pure a
throwHResult (hres, _)  = throwError hres
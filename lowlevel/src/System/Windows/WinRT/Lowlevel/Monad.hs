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
   throwHResult',
   CULong(..),
   e_no_interface
) where

import Control.Monad.Except
import Foreign.C.Types

e_no_interface :: HRESULT
e_no_interface = 0x80004002

type HRESULT = CULong
type WinRTCore a = ExceptT HRESULT IO a
type WinRTUnwrapped a = IO (Either HRESULT a)

newtype WinRT a = WinRT { unWinRT :: WinRTCore a}
   deriving (Functor, Applicative, Monad, MonadIO, MonadError HRESULT)

passthrough :: ((a → WinRTUnwrapped b) → WinRTUnwrapped b) → (a → WinRT b) → WinRT b
passthrough f g = wrap $ f (unwrap . g)

passthrough2 :: ((a → b → WinRTUnwrapped c) → WinRTUnwrapped c) → (a → b → WinRT c) → WinRT c
passthrough2 f g = wrap $ f (\a b → unwrap $ g a b)

wrap :: WinRTUnwrapped a → WinRT a
wrap = WinRT . ExceptT

unwrap :: WinRT a → WinRTUnwrapped a
unwrap = runExceptT . unWinRT

try :: IO HRESULT → WinRT ()
try it = liftIO it >>= flip throwHResult' () 

throwHResult' ∷ HRESULT → a → WinRT a
throwHResult' = curry throwHResult

throwHResult ∷ (HRESULT, a) → WinRT a
throwHResult (0, a)  = pure a
throwHResult (hres, _)  = throwError hres
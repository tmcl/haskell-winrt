{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Windows.WinRT.Monad where

import Control.Monad.Except
import System.Win32.Types (HRESULT)

type WinRTCore a = ExceptT HRESULT IO a

newtype WinRT a = WinRT { unWinRT :: WinRTCore a}
   deriving (Functor, Applicative, Monad, MonadIO, MonadError HRESULT)

throwHResult ∷ (HRESULT, a) → WinRT a
throwHResult (hres, a) 
   | hres >= 0x8000_0000 = throwError hres
   | otherwise = pure a
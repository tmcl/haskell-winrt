{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Windows.WinRT
import System.Windows.WinRT.RoInit
import Control.Monad.IO.Class
import qualified Windows.UI.Xaml.Application
import Windows.UI.Xaml.Application.IApplicationStatics

main :: IO ()
main = do
   res <- run it
   print res

it :: WinRT ()
it = do
   liftIO $ putStrLn "initialised"
   statics :: ApplicationStatics ← getActivationFactory Windows.UI.Xaml.Application.classId
   liftIO $ putStrLn "got the statics"



   -- insp ← newInspectable $ pack "Windows.UI.Xaml.Application"
   -- app ← queryInterface 
   -- liftIO $ putStrLn "created"
   




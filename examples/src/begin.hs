module Main where

import System.Windows.WinRT
import Control.Monad.IO.Class

main :: IO ()
main = do
   res <- run it
   print res

it :: WinRT ()
it = do
   liftIO $ putStrLn "initialised"
   insp ← newInspectable $ pack "Windows.UI.Xaml.Application"
   app ← queryInterface 
   liftIO $ putStrLn "created"
   




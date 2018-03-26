module Main where

import SPI
import System.Environment

main :: IO ()
main = do [fileName] <- getArgs
          c <- readFile fileName
          putStrLn $ show $ snd $ runIdentity $ (runStateT (interpret (parse c)) initSymbolTable)

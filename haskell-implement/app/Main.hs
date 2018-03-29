module Main where

import SPI
import System.Environment
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as Map
import Text.Printf

main :: IO ()
main = do [fileName] <- getArgs
          c <- readFile fileName
          st <- return $ snd $ runIdentity $ (runStateT (interpret (parse c)) initSymbolTable)
          putStrLn "Symbol Table contents:"
          printf "Symbols: %s\n" (show $ Map.elems st)
          putStrLn ""
          putStrLn "Run-time GLOBAL_MEMORY contents:"
          mapM_ (\(k,v) -> putStrLn $ printf "%s = %s" k (showValue v)) $ sortBy (on compare fst) (Map.toList st)

module Main where

import SPI
import System.Environment
import Data.Function
import Data.List

main :: IO ()
main = do [fileName] <- getArgs
          c <- readFile fileName
          st <- return $ snd $ runIdentity $ (runStateT (interpret (parse c)) initSymbolTable)
          mapM_ (\(k,v) -> putStrLn $ k ++ " = " ++ (show v)) $ sortBy (on compare fst) st

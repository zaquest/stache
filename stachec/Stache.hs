{-# LANGUAGE CPP #-}
module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Text.Stache.Types
import Text.Stache.Scan (scan)
import Text.Stache.Parse (parse)
#if defined(PHP)
import Text.Stache.Php (compile)
#elif defined(JS)
import Text.Stache.JavaScript (compile)
#else
#error Supported languages: JS, PHP
#endif

main :: IO ()
main = do
  [file] <- getArgs
  str <- T.readFile file
  let tmpl = parse (scan str)
  T.putStr (compile tmpl)

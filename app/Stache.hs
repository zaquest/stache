{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import Text.Stache.Compile
import Text.Stache.Scan (scan)
import Text.Stache.Parse (parse)
import Text.Stache.Php (phpCodeGen)
import Text.Stache.JavaScript (jsCodeGen)

compile' :: CodeGen -> FilePath -> IO ()
compile' cg file = do
  tmpl <- parse . scan <$> T.readFile file
  T.putStr (compile cg tmpl)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["js", file] -> compile' jsCodeGen file
    ["php", file] -> compile' phpCodeGen file
    _ -> T.putStrLn "Usage: stachec (js|php) template_file"

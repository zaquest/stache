{-# LANGUAGE OverloadedStrings #-}
module Text.Stache.JavaScript (jsCodeGen) where

import Data.Char (isControl)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (foldl1, traverse_)
import Control.Monad.Writer.Strict (Writer)
import qualified Control.Monad.Writer.Strict as Wr
import Text.Stache.Compile

escape :: Text -> Text
escape s = case T.uncons t of
             Nothing -> h
             Just (c, t') -> h <> escapeChar c <> escape t'
  where (h, t) = T.break (\c -> c == '\'' || isControl c) s
        escapeChar '\'' = "\\\'"
        escapeChar c = T.pack . take 2 . drop 1 $ show c -- hax

jsRaw :: Text -> Path -> Text -- root -> path -> code
jsRaw root path = root <> "." <> join "." path

jsEscape :: Text -> Text -- encoded path -> code
jsEscape path = "e(" <> path <> ")"

jsLit :: Literal -> Text -- string -> literal string
jsLit s = "'" <> escape s <> "'"

jsIf :: [(Text, [Text])] -> [Text] -> Text -- [(encoded path, [case body chunk])] -> [else body chunk] -> code
jsIf [] ecs = "(" <> join " + " ecs <> ")"
jsIf ((p, cs) : css) ecs = "(" <> p <> " ? (" <> join " + " cs <> ") : " <> jsIf css ecs <> ")"

jsEach :: Text -> Proto -> Text -- encoded path -> function name -> code
jsEach path proto = path <> ".map(" <> name proto <> ").join('')"

jsVar :: Id -> Text -- function id -> function name
jsVar n = "p" <> (T.pack $ show n)

jsFunction :: Proto -> [Fn] -> [Text] -> Text -- proto -> [fn deps] -> [body chunk] -> code
jsFunction proto _ chunks = "var " <> name proto <> " = " <>
  "function (" <> arg proto <> ") {return " <> join " + " chunks <> ";};"

jsModule :: Proto -> Text -> Text -- entry point -> body -> code
jsModule proto code = "define(['escapeHTML'], function (e) {" <> code <> "return " <> name proto <> "; });"

jsCodeGen :: CodeGen
jsCodeGen = CodeGen
  { genRaw = jsRaw
  , genEscape = jsEscape
  , genLit = jsLit
  , genIf = jsIf
  , genEach = jsEach
  , genVar = jsVar
  , genFunction = jsFunction
  , genModule = jsModule }

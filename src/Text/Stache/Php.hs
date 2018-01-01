{-# LANGUAGE OverloadedStrings #-}
module Text.Stache.Php (phpCodeGen) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Stache.Compile

escape :: Text -> Text
escape s = case T.uncons t of
             Nothing -> h
             Just (c, t') -> h <> escapeChar c <> escape t'
  where (h, t) = T.break (\c -> c == '\'') s
        escapeChar '\'' = "\\\'"

phpRaw :: Text -> Path -> Text
phpRaw root path = "$" <> root <> "['" <> join "']['" path <> "']"

phpEscape :: Text -> Text
phpEscape path = "htmlspecialchars(" <> path <> ")"

phpLit :: Literal -> Text
phpLit s = "'" <> escape s <> "'"

phpIf :: [(Text, [Text])] -> [Text] -> Text
phpIf [] ecs = "(" <> join " . " ecs <> ")"
phpIf ((p, cs) : css) ecs = "(" <> p <> " ? (" <> join " . " cs <> ") : " <> phpIf css ecs <> ")"

phpEach :: Text -> Proto -> Text
phpEach path proto = "join('', array_map(" <> name proto <> ", " <> path <> "))"

phpVar :: Id -> Text
phpVar n = "$p" <> (T.pack $ show n)

phpFunction :: Proto -> [Fn] -> [Text] -> Text
phpFunction proto' deps chunks = name proto' <> " = " <>
  "function ($" <> arg proto' <> ")" <> use <> "{return " <> join " . " chunks <> ";};\n"
    where use | null deps = ""
              | otherwise = " use (" <> join ", " (map (name . proto) deps) <> ") "

phpModule :: Proto -> Text -> Text
phpModule main code = "<?php return (function () {\n" <> code <> "return " <> name main <> ";\n})();\n"

phpCodeGen :: CodeGen
phpCodeGen = CodeGen
  { genRaw = phpRaw
  , genEscape = phpEscape
  , genLit = phpLit
  , genIf = phpIf
  , genEach = phpEach
  , genVar = phpVar
  , genFunction = phpFunction
  , genModule = phpModule }

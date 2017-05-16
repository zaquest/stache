{-# LANGUAGE OverloadedStrings #-}
module Text.Stache.Php where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (foldl1, traverse_)
import Control.Monad.Writer.Strict (Writer)
import qualified Control.Monad.Writer.Strict as Wr
import Text.Stache.Types

varPref :: Text
varPref = "$p"

var :: Int -> Text
var = (varPref <>) . T.pack . show

type Code = Text

genCode :: Template Id -> (Id, Code)
genCode = Wr.runWriter . genCodeT

type CodeGen a = Writer Code a

genCodeT :: Template Id -> CodeGen Id
genCodeT (T id xs) = do
  mapM_ (traverse_ genCodeT) xs
  Wr.tell $ renderFn id (getDeps xs) str
  pure id
    where cs = map genCodeB xs
          str = join " . " cs
          getDeps = foldMap (foldMap (pure . label))

genCodeB :: Base (Template Id) -> Code
genCodeB (Raw p) = joinPath p
genCodeB (Escape p) = "htmlspecialchars(" <> joinPath p <> ")"
genCodeB (Lit lit) = phpString lit
genCodeB (If cs) = compileIf (map (fmap label) cs) "''"
genCodeB (IfElse cs t) = compileIf (map (fmap label) cs) (call (label t))
genCodeB (Each p t) = "join('', array_map(" <> (var (label t)) <> ", " <> joinPath p <> "))"

compileIf :: [(Path, Id)] -> Code -> Code
compileIf cases elseCase = build (map (fmap call) cases)
  where build [] = elseCase
        build ((pred, ifCase) : xs) = "(" <> joinPath pred <> " ? " <> ifCase <> " : " <> build xs <> ")"

call :: Id -> Code
call fn = var fn <> "(" <> root <> ")"

root :: Text
root = "$d"

joinPath :: Path -> Text
joinPath path = root <> "['" <> join "']['" path <> "']"

escape :: Text -> Text
escape s = case T.uncons t of
             Nothing -> h
             Just (c, t') -> h <> escapeChar c <> escape t'
  where (h, t) = T.break (\c -> c == '\'') s
        escapeChar '\'' = "\\\'"

phpString :: Text -> Text
phpString s = "'" <> escape s <> "'"

renderFn :: Id -> [Id] -> Code -> Code
renderFn id vars chunks = var id <> " = " <>
  "function (" <> root <> ") " <> use <> " {\n"
  <> "return " <> chunks <> ";\n};"
    where
      use | null vars = ""
          | otherwise = "use (" <> join ", " (map var vars) <> ")"

wrapModule :: (Id, Code) -> Code
wrapModule (id, code) = "<?php return (function () {\n" <> code <> "return " <> var id <> ";\n})();\n"

compile :: Template () -> Text
compile = wrapModule . genCode . name

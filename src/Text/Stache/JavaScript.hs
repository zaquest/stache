{-# LANGUAGE OverloadedStrings #-}
module Text.Stache.JavaScript (compile) where

import Data.Char (isControl)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (foldl1, traverse_)
import Control.Monad.Writer.Strict (Writer)
import qualified Control.Monad.Writer.Strict as Wr
import Text.Stache.Types

varPref :: Text
varPref = "p"

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
          str = join " + " cs
          getDeps = foldMap (foldMap (pure . label))

genCodeB :: Base (Template Id) -> Code
genCodeB (Raw p) = joinPath p
genCodeB (Escape p) = "e(" <> joinPath p <> ")"
genCodeB (Lit lit) = jsString lit
genCodeB (If cs) = compileIf (map (fmap label) cs) "''"
genCodeB (IfElse cs t) = compileIf (map (fmap label) cs) (call (label t))
genCodeB (Each p t) = joinPath p <> ".map(" <> var (label t) <> ").join('')"

compileIf :: [(Path, Id)] -> Code -> Code
compileIf cases elseCase = build (map (fmap call) cases)
  where build [] = elseCase
        build ((pred, ifCase) : xs) = "(" <> joinPath pred <> " ? " <> ifCase <> " : " <> build xs <> ")"
-- compileIf :: [(Path, [Template])] -> Fn Chunk
-- compileIf cases = do
--   calls <- map call <$>  mapM (define . compileFn) subs
--   pure (join " : " (zipWith (\p c -> joinP p <> " ? " <> c) paths calls))
--   where (paths, subs) = unzip cases
--         call fn = fn <> "(" <> root <> ")"

call :: Id -> Code
call fn = var fn <> "(" <> root <> ")"

root :: Text
root = "d"

joinPath :: Path -> Text
joinPath = join "." . (root :)

escape :: Text -> Text
escape s = case T.uncons t of
             Nothing -> h
             Just (c, t') -> h <> escapeChar c <> escape t'
  where (h, t) = T.break (\c -> c == '\'' || isControl c) s
        escapeChar '\'' = "\\\'"
        escapeChar c = T.pack . take 2 . drop 1 $ show c -- hax

jsString :: Text -> Text
jsString s = "'" <> escape s <> "'"

renderFn :: Id -> [Id] -> Code -> Code
renderFn id _ chunks = "var " <> var id <> " = " <>
  "function (" <> root <> ") {\nreturn " <> chunks <> ";\n};"

wrapModule :: (Id, Code) -> Code
wrapModule (id, code) = "define(['escapeHTML'], function (e) {" <> code <> "return " <> var id <> "; });"

compile :: Template () -> Text
compile = wrapModule . genCode . name

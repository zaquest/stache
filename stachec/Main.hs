{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.Environment (getArgs)
import Data.Monoid
import Data.Char (isControl)
import qualified Data.List as List
import Data.Foldable (foldl1)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as St
import Control.Monad.Writer.Strict (WriterT)
import qualified Control.Monad.Writer.Strict as Wr
import Control.Monad.Trans (lift)
import Data.Sequence (Seq, ViewL(..), (<|), (|>))
import qualified Data.Sequence as Seq
import Text.Stache.Types
import Text.Stache.Scan (scan)
import Text.Stache.Parse (parse, Template(..))

type Var = Text -- variable name
type Value = Text -- RHS of an assignment
type Chunk = Text
type Def = (Var, Text) -- partials

newtype Fn a = Fn { unFn :: WriterT (Seq Def, Seq Chunk) (State Int) a }
  deriving (Functor, Applicative, Monad)

define :: Fn () -> Fn Var
define val = Fn $ do
  (defs, fn) <- unFn $ finishFn val
  n <- St.state $ \n -> (n, n+1)
  let v = var n
  St.put (n + 1)
  Wr.tell (defs |> (v, fn), mempty)
  pure v

tell :: Chunk -> Fn ()
tell chunk = Fn $ Wr.tell (mempty, Seq.singleton chunk)

root :: Text
root = "d"

join :: (Monoid a, Foldable t) => a -> t a -> a
join sep = foldl1 (\l r -> l <> sep <> r)

joinP :: Path -> Text
joinP = join "." . (root :)

escape :: Text -> Text
escape s = case T.uncons t of
             Nothing -> h
             Just (c, t') -> h <> escapeChar c <> escape t'
  where (h, t) = T.break (\c -> c == '"' || isControl c) s
        escapeChar '"' = "\\\""
        escapeChar c = T.pack . take 2 . drop 1 $ show c -- hax

jsString :: Text -> Text
jsString s = "\"" <> escape s <> "\""

varPref :: Text
varPref = "p"

var :: Int -> Text
var = (varPref <>) . T.pack . show

renderDef :: Var -> Value -> Text
renderDef name val = "var " <> name <> " = " <> val <> ";"

compile :: [Template] -> (Text, Text)
compile tmpl = (foldMap (uncurry renderDef) defs, wrapFn func)
  where Fn fn = compileFn tmpl
        (defs, func) = St.evalState (Wr.execWriterT fn) 0

finishFn :: Fn a -> Fn (Seq Def, Text)
finishFn (Fn fn) = Fn . lift $ fmap wrapFn <$> Wr.execWriterT fn

wrapFn :: Seq Chunk -> Text
wrapFn chunks = "function (" <> root <> ") {" <> "return "
                <> join " + " chunks <> ";}"

compileFn :: [Template] -> Fn ()
compileFn = mapM_ compileChunk

compileChunk :: Template -> Fn ()
compileChunk (Raw path) = tell (joinP path)
compileChunk (Lit str) = tell (jsString str)
compileChunk (Escape path) = tell ("e(" <> joinP path <> ")")
compileChunk (Each path sub) = do
  var <- define (compileFn sub)
  tell (joinP path <> ".map(" <> var <> ").join('')")
compileChunk (If cases) = do
  ifs <- compileIf cases
  tell ("(" <> ifs <> " : '')")
compileChunk (IfElse cases elseCase) = do
  ifs <- compileIf cases
  var <- define (compileFn elseCase)
  tell ("(" <> ifs <> " : " <> var <> "(" <> root <> "))")

compileIf :: [(Path, [Template])] -> Fn Chunk
compileIf cases = do
  calls <- map call <$>  mapM (define . compileFn) subs
  pure (join " : " (zipWith (\p c -> joinP p <> " ? " <> c) paths calls))
  where (paths, subs) = unzip cases
        call fn = fn <> "(" <> root <> ")"

wrapAMD :: (Text, Text) -> Text
wrapAMD (defs, fn) = "define(['escapeHTML'], function (e) {" <> defs <> "return " <> fn <> "; });"

main :: IO ()
main = do
  [file] <- getArgs
  str <- T.readFile file
  let tmpl = parse (scan str)
  T.putStr (wrapAMD $ compile tmpl)

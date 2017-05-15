{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Stache.Php (compile) where

import Data.Monoid
import Data.Foldable (foldl1)
import Data.Text (Text)
import qualified Data.Text as T
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
  Wr.tell (defs |> (v, fn), mempty)
  pure v

tell :: Chunk -> Fn ()
tell chunk = Fn $ Wr.tell (mempty, Seq.singleton chunk)

finishFn :: Fn a -> Fn (Seq Def, Text)
finishFn (Fn fn) = Fn . lift $ (\(defs, chunks) -> (defs, renderFn (fmap fst defs) chunks)) <$> Wr.execWriterT fn

root :: Text
root = "$d"

join :: (Monoid a, Foldable t) => a -> t a -> a
join sep = foldl1 (\l r -> l <> sep <> r)

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

varPref :: Text
varPref = "$p"

var :: Int -> Text
var = (varPref <>) . T.pack . show

renderDef :: Var -> Value -> Text
renderDef name val = name <> " = " <> val <> ";\n"

compileAll :: [Template] -> (Text, Text)
compileAll tmpl = (foldMap (uncurry renderDef) defs, renderFn (fmap fst defs) func)
  where Fn fn = compileFn tmpl
        (defs, func) = St.evalState (Wr.execWriterT fn) 0

compileFn :: [Template] -> Fn ()
compileFn = mapM_ compileChunk

renderFn :: Seq Var -> Seq Chunk -> Text
renderFn vars chunks =
  "function (" <> root <> ") " <> use <> " {\n"
  <> "return "
  <> join " . " chunks <> ";\n}"
    where
      use | Seq.null vars = ""
          | otherwise = "use (" <> join ", " vars <> ")"

compileChunk :: Template -> Fn ()
compileChunk (Raw path) = tell (joinPath path)
compileChunk (Lit str) = tell (phpString str)
compileChunk (Escape path) = tell ("htmlspecialchars(" <> joinPath path <> ")")
compileChunk (Each path sub) = do
  var <- define (compileFn sub)
  tell ("join('', array_map(" <> var <> ", " <> joinPath path <> "))")
compileChunk (If cases) = do
  ifs <- compileIf cases "''"
  tell ifs
compileChunk (IfElse cases elseCase) = do
  var <- define (compileFn elseCase)
  ifs <- compileIf cases (var <> "(" <> root <> ")")
  tell ifs

compileIf :: [(Path, [Template])] -> Chunk -> Fn Chunk
compileIf cases elseCase = do
  calls <- map call <$>  mapM (define . compileFn) subs
  pure (build (zip paths calls))
  where (paths, subs) = unzip cases
        build [] = elseCase
        build ((pred, ifCase) : xs) = "(" <> joinPath pred <> " ? " <> ifCase <> " : " <> build xs <> ")"
        call fn = fn <> "(" <> root <> ")"

wrapModule :: (Text, Text) -> Text
wrapModule (defs, fn) = "<?php return (function () {\n" <> defs <> "return " <> fn <> ";\n})();\n"

compile :: [Template] -> Text
compile tmpl = wrapModule (compileAll tmpl)

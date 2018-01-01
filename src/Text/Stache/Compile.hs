{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Text.Stache.Compile where

import Data.Text (Text)
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as St
import Data.Semigroup (Semigroup(..), (<>))
import Data.Bifunctor (bimap)

type Literal = Text
type Path = [Text]

data Base t = Raw Path
            | Escape Path
            | Lit Literal
            | If [(Path, [Base t])] [Base t]
            | Each Path t
  deriving (Show, Functor, Foldable, Traversable)

data Template a = T { label :: a, datum :: [Base (Template a)] }
  deriving (Show, Functor, Foldable, Traversable)

type Id = Int
type IdSrc a = State Int a

next :: IdSrc Id
next = St.state $ \n -> (n, n+1)

named :: Template () -> Template Id
named t = St.evalState (traverse (const next) t) 0

join :: (Semigroup a, Foldable t) => a -> t a -> a
join sep = foldl1 (\l r -> l <> sep <> r)

data Proto = Proto
  { name :: Text
  , arg :: Text
  } deriving (Show)

data Fn = Fn
  { proto :: Proto
  , deps :: [Fn]
  , code :: Text
  } deriving (Show)

data CodeGen = CodeGen
  { genRaw :: Text -> Path -> Text -- root -> path -> code
  , genEscape :: Text -> Text -- encoded path -> code
  , genLit :: Literal -> Text -- string -> literal string
  , genIf :: [(Text, [Text])] -> [Text] -> Text -- [(encoded path, [case body chunk])] -> [else body chunk] -> code
  , genEach :: Text -> Proto -> Text -- encoded path -> function name -> code
  , genVar :: Id -> Text -- function id -> function name
  , genFunction :: Proto -> [Fn] -> [Text] -> Text -- proto -> [fn deps] -> [body chunk] -> code
  , genModule :: Proto -> Text -> Text -- entry point -> body -> code
  }

compile :: CodeGen -> Template () -> Text
compile cg t =
  let fn = goT (named t)
   in genModule cg (proto fn) (emit fn)
  where
    goB proto (Raw path) = (genRaw cg (arg proto) path, [])
    goB proto (Escape path) = (genEscape cg (genRaw cg (arg proto) path), [])
    goB proto (Lit lit) = (genLit cg lit, [])
    goB proto (If cases elseCase) =
      let casesCode' = bimap (genRaw cg (arg proto)) (unzip . map (goB proto)) <$> cases
          (casesCode, casesDeps) = unzip $ map (\(p, (c, d)) -> ((p, c), concat d)) casesCode'
          (elseCode, elseDeps) = unzip $ map (goB proto) elseCase
       in (genIf cg casesCode elseCode, concat casesDeps ++ concat elseDeps)
    goB proto' (Each path t) =
      let fn = goT t
          path' = genRaw cg (arg proto') path
       in (genEach cg path' (proto fn), [fn])
    goT t =
      let proto = Proto (genVar cg (label t)) "d"
          (code', deps') = unzip $ map (goB proto) (datum t)
          deps = concat deps'
          code = genFunction cg proto deps code'
       in Fn proto deps code
    emit (Fn _ xs code) = mconcat (map emit xs) <> code

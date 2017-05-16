{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Text.Stache.Types where

import Data.Text (Text)
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as St
import Data.Semigroup (Semigroup(..), (<>))

type Literal = Text
type Path = [Text]

data Base t = Raw Path
            | Escape Path
            | Lit Literal
            | If [(Path, t)]
            | IfElse [(Path, t)] t
            | Each Path t
  deriving (Show, Functor, Foldable, Traversable)

data Template a = T { label :: a, datum :: [Base (Template a)] }
  deriving (Show, Functor, Foldable, Traversable)

type Id = Int
type IdSrc a = State Int a

next :: IdSrc Id
next = St.state $ \n -> (n, n+1)

name :: Template () -> Template Id
name t = St.evalState (traverse (const next) t) 0

join :: (Semigroup a, Foldable t) => a -> t a -> a
join sep = foldl1 (\l r -> l <> sep <> r)

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Arrows #-}

module Opalist where

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Control.Applicative as A
import qualified Control.Arrow as Arr
import qualified Data.Map as Map
import qualified Data.Monoid as M
import           Data.Monoid ((<>))
import qualified Data.List as L
import qualified Foldl

type Query a = QueryArr () a
type QueryArr a b = Arr.Kleisli [] a b

-- { Aggregation

data Aggregator a b =
  forall r r'. Ord r => Aggregator (a -> r) (Foldl.Fold a r') ((r, r') -> b)

-- The instances are almost entirely boilerplate

instance P.Profunctor Aggregator where
  dimap f g (Aggregator group fold finish) =
    Aggregator (P.lmap f group) (P.lmap f fold) (fmap g finish)

instance Functor (Aggregator a) where
  fmap f (Aggregator group fold finish) = Aggregator group fold (fmap f finish)

instance A.Applicative (Aggregator a) where
  pure x = Aggregator (A.pure ()) (A.pure ()) (A.pure x)
  Aggregator groupf foldf finishf <*> Aggregator groupx foldx finishx =
    Aggregator (groupf Arr.&&& groupx)
               ((,) A.<$> foldf A.<*> foldx)
               (\((rf, rx), (r'f, r'x)) -> finishf (rf, r'f) (finishx (rx, r'x)))

instance PP.ProductProfunctor Aggregator where
  empty  = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg = listQuery . aggregateList agg . runQuery

aggregateList :: Aggregator a b -> [a] -> [b]
aggregateList (Aggregator group fold finish) =
  map finish
  . Map.toList
  . fmap (Foldl.runFold fold)
  . Map.fromListWith (++)
  . map (group Arr.&&& (:[]))

groupBy :: Ord a => Aggregator a a
groupBy = Aggregator id (A.pure ()) fst

-- }

-- { Adapting Folds to Aggregators

fromFoldl :: Foldl.Fold a b -> Aggregator a b
fromFoldl foldl = Aggregator (A.pure ()) foldl snd

sumA :: Num a => Aggregator a a
sumA = fromFoldl Foldl.sumF

maxA :: Ord a => Aggregator a (Maybe a)
maxA = fromFoldl Foldl.maximumF

countA :: Aggregator a Int
countA = fromFoldl Foldl.lengthF

-- }

-- { Ordering

data Order a = Order (a -> a -> Ordering)

asc :: Ord b => (a -> b) -> Order a
asc f = Order (\x y -> compare (f x) (f y))

desc :: Ord b => (a -> b) -> Order a
desc f = Order (\x y -> compare (f y) (f x))

orderBy :: Order a -> Query a -> Query a
orderBy (Order compare') = listQuery .L.sortBy compare' . runQuery

instance M.Monoid (Order a) where
  mempty = Order (const (const EQ))
  Order f `mappend` Order g = Order (f `M.mappend` g)

-- }

-- { Isomorphism between [] and Query

listQuery :: [a] -> Query a
listQuery = Arr.Kleisli . const

runQuery :: Query a -> [a]
runQuery = ($ ()) . Arr.runKleisli

-- }

printRows :: Show a => [a] -> IO ()
printRows = mapM_ print

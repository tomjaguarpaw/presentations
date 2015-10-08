{-# LANGUAGE ExistentialQuantification #-}

module Foldl where

import qualified Data.Profunctor as P
import qualified Control.Applicative as A

data Fold a b
  -- | @Fold @ @ step @ @ initial @ @ extract@
  = forall x. Fold (x -> a -> x) x (x -> b)

data Pair a b = Pair !a !b

-- | Apply a strict left 'Fold' to a 'Foldable' container
runFold :: Fold a b -> [a] -> b
runFold (Fold step begin done) as = foldr cons done as begin
  where
    cons a k x = k $! step x a

premap :: (a -> b) -> Fold b r -> Fold a r
premap f (Fold step begin done) = Fold step' begin done
  where
    step' x a = step x (f a)

sumF :: Num a => Fold a a
sumF = Fold (+) 0 id

maximumF :: Ord a => Fold a (Maybe a)
maximumF = Fold max' Nothing id
  where Nothing `max'` a  = Just a
        Just a  `max'` a' = Just (a `max` a')

lengthF :: Fold a Int
lengthF = Fold (\n _ -> n + 1) 0 id

instance Functor (Fold a) where
    fmap f (Fold step begin done) = Fold step begin (f . done)
    {-# INLINABLE fmap #-}

instance P.Profunctor Fold where
    lmap = premap
    rmap = fmap

instance A.Applicative (Fold a) where
    pure b    = Fold (\() _ -> ()) () (\() -> b)

    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            begin = Pair beginL beginR
            done (Pair xL xR) = doneL xL (doneR xR)
        in  Fold step begin done




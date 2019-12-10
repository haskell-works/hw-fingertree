{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.FingerTreeSpec (spec) where

import Control.Applicative          (Applicative (..))
import Control.Monad                (ap)
import Data.Foldable                (Foldable (foldMap, foldl, foldr), all, toList)
import Data.Functor                 ((<$>))
import Data.List                    (inits)
import Data.Monoid                  (Monoid (..))
import Data.Traversable             (traverse)
import HaskellWorks.Data.FingerTree
import HaskellWorks.Hspec.Hedgehog
import Hedgehog                     hiding (evalM)
import Prelude                      hiding (null, reverse)
import Test.Hspec

import qualified HaskellWorks.Data.FingerTree.Gen as G
import qualified Hedgehog.Gen                     as G
import qualified Hedgehog.Range                   as R
import qualified Prelude                          as P

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = do
  it "foldr" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    foldr (:) [] xs === P.foldr (:) [] (toList xs)
  it "foldl" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    foldl (flip (:)) [] xs === P.foldl (flip (:)) [] (toList xs)
  it "(==)" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    ys <- forAll (G.fingerTree (G.int R.constantBounded))
    (xs == ys) === (toList xs == toList ys)
  it "compare" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    ys <- forAll (G.fingerTree (G.int R.constantBounded))
    compare xs ys === compare (toList xs) (toList ys)
  it "mappend" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    ys <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (mappend xs ys) ~== toList xs ++ toList ys
  it "empty" $ require $ property $ do
    toList' (empty :: Seq Int) === Just []
  it "singletone" $ require $ property $ do
    x <- forAll (G.int R.constantBounded)
    toList' (singleton x) ~== [x]
  it "(<|)" $ require $ property $ do
    x  <- forAll (G.int R.constantBounded)
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (x <| xs) ~== x : toList xs
  it "(|>)" $ require $ property $ do
    x  <- forAll (G.int R.constantBounded)
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (xs |> x) ~== toList xs ++ [x]
  it "(><)" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    ys <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (xs >< ys) ~== toList xs ++ toList ys
  it "fromList" $ require $ property $ do
    xs <- forAll (G.list (R.linear 0 100) (G.int R.constantBounded))
    toList' (fromList xs) ~== xs
  it "null" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    null xs === P.null (toList xs)
  it "viewl" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    case viewl xs of
      EmptyL    -> P.null (toList xs) === True
      x :< xs'  -> do
        valid xs' === True
        toList xs === x : toList xs'
  it "viewr" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    case viewr xs of
      EmptyR    -> P.null (toList xs) === True
      xs' :> x  -> do
        valid xs' === True
        toList xs === toList xs' ++ [x]
  it "split" $ require $ property $ do
    n <- forAll (G.int R.constantBounded)
    let p ys = P.length ys > n
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toListPair' (split p xs) ~== P.splitAt n (toList xs)
  it "takeUntil" $ require $ property $ do
    n <- forAll (G.int R.constantBounded)
    let p ys = P.length ys > n
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (takeUntil p xs) ~== P.take n (toList xs)
  it "dropUntil" $ require $ property $ do
    n <- forAll (G.int R.constantBounded)
    let p ys = P.length ys > n
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (dropUntil p xs) ~== P.drop n (toList xs)
  it "reverse" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (reverse xs) ~== P.reverse (toList xs)
  it "fmap" $ require $ property $ do
    let f = Just
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (fmap' f xs) ~== map f (toList xs)
  it "fmapWithPos" $ require $ property $ do
    let f = (,)
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    let xs_list = toList xs
    toList' (fmapWithPos f xs) ~== zipWith f (inits xs_list) xs_list
  it "traverse" $ require $ property $ do
    let f x = do
          n <- step
          return (n, x)
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    toList' (evalM (traverse' f xs)) ~== evalM (traverse f (toList xs))
  it "traverseWithPos" $ require $ property $ do
    xs <- forAll (G.fingerTree (G.int R.constantBounded))
    let f ys y = do
          n <- step
          return (ys, n, y)
    let xs_list = toList xs
    toList' (evalM (traverseWithPos f xs)) ~== evalM (traverse (uncurry f) (zip (inits xs_list) xs_list))

infix 4 ~==

(~==) :: (Show a, Eq a) => Maybe a -> a -> PropertyT IO ()
(~==) = maybe (const failure) (===)

newtype M a = M (Int -> (Int, a))

runM :: M a -> Int -> (Int, a)
runM (M m) = m

evalM :: M a -> a
evalM m = snd (runM m 0)

instance Monad M where
    return x = M $ \ n -> (n, x)
    M u >>= f = M $ \ m -> let (n, x) = u m in runM (f x) n

instance Functor M where
    fmap f (M u) = M $ \ m -> let (n, x) = u m in (n, f x)

instance Applicative M where
    pure = return
    (<*>) = ap

step :: M Int
step = M $ \ n -> (n+1, n)

toListPair' ::
    (Eq a, Measured [a] a, Valid a, Eq b, Measured [b] b, Valid b) =>
        (Seq a, Seq b) -> Maybe ([a], [b])
toListPair' (xs, ys) = (,) <$> toList' xs <*> toList' ys

toList' :: (Eq a, Measured [a] a, Valid a) => Seq a -> Maybe [a]
toList' xs
  | valid xs = Just (toList xs)
  | otherwise = Nothing

class Valid a where
  valid :: a -> Bool

instance (Measured v a, Eq v, Valid a) => Valid (FingerTree v a) where
    valid Empty = True
    valid (Single x) = valid x
    valid (Deep s pr m sf) =
        s == measure pr `mappend` measure m `mappend` measure sf &&
        valid pr && valid m && valid sf

instance (Measured v a, Eq v, Valid a) => Valid (Node v a) where
    valid node = measure node == foldMap measure node && all valid node

instance Valid a => Valid (Digit a) where
    valid = all valid

instance Valid Int where
    valid = const True

instance Valid (a,b) where
    valid = const True

instance Valid (a,b,c) where
    valid = const True

instance Valid (Maybe a) where
    valid = const True

instance Valid [a] where
    valid = const True

------------------------------------------------------------------------
-- Use list of elements as the measure
------------------------------------------------------------------------

type Seq a = FingerTree [a] a

instance Measured [Int] Int where
    measure x = [x]

instance Measured [Maybe a] (Maybe a) where
    measure x = [x]

instance Measured [(a, b)] (a, b) where
    measure x = [x]

instance Measured [(a, b, c)] (a, b, c) where
    measure x = [x]

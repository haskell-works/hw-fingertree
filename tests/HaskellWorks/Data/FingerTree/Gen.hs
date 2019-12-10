{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Data.FingerTree.Gen where

import Control.Monad
import HaskellWorks.Data.FingerTree
import Hedgehog

import qualified Hedgehog.Gen             as G
import qualified Hedgehog.Internal.Gen    as G
import qualified Hedgehog.Internal.Shrink as S
import qualified Hedgehog.Range           as R

genList :: MonadGen m => Range Int -> m a -> m [a]
genList range gen =
  G.sized $ \size ->
    (traverse snd =<<) .
    G.ensure (G.atLeast $ R.lowerBound size range) .
    G.shrink S.list $ do
      k <- G.integral_ range
      replicateM k (G.freeze gen)

shrinkFingerTree :: Measured v a => FingerTree v a -> [FingerTree v a]
shrinkFingerTree (Deep _ (One a) Empty (One b)) = [Single a, Single b]
shrinkFingerTree (Deep _ pr m sf) =
    [deep pr' m  sf  | pr' <- shrinkDigit      pr] ++
    [deep pr  m' sf  | m'  <- shrinkFingerTree m ] ++
    [deep pr  m  sf' | sf' <- shrinkDigit      sf]
shrinkFingerTree (Single _) = []
shrinkFingerTree Empty      = []

fingerTree :: (MonadGen m, Measured v a) => m a -> m (FingerTree v a)
fingerTree gen = G.sized $ \size -> genSizedFingerTree size gen

genSizedFingerTree :: (MonadGen m, Measured v a) => Size -> m a -> m (FingerTree v a)
genSizedFingerTree n gen = G.shrink shrinkFingerTree $ case n of
    0 -> return Empty
    1 -> Single <$> gen
    o -> deep <$> (One <$> gen) <*> genSizedFingerTree (o `div` 2) (genSizedNode (o `div` 2) gen) <*> (One <$> gen)

shrinkNode :: Measured v a => Node v a -> [Node v a]
shrinkNode (Node2 _ _ _  ) = []
shrinkNode (Node3 _ a b c) = [node2 a  b, node2 a c, node2 b c]

genSizedNode :: (MonadGen m, Measured v a) => Size -> m a -> m (Node v a)
genSizedNode n gen = G.shrink shrinkNode $ G.choice
    [ node2 <$> gen <*> gen
    , node3 <$> gen <*> gen <*> gen
    ]

shrinkDigit :: Digit a -> [Digit a]
shrinkDigit (One    _      ) = []
shrinkDigit (Two    a b    ) = [One a, One b]
shrinkDigit (Three  a b c  ) = [Two a b, Two a c, Two b c]
shrinkDigit (Four   a b c d) = [Three a b c, Three a b d, Three a c d, Three b c d]

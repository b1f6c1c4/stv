{-# LANGUAGE TupleSections #-}

module Core
    ( Vote (..)
    , VoteCount (..)
    , countVotes
    , splitTier
    , elect
    , eliminate
    ) where

import Data.List (sortOn)
import Data.List.Extra (sumOn')
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.Ratio
import Numeric

data Vote a = Vote {weight :: Rational, choices :: NE.NonEmpty a}

instance (Show a) => Show (Vote a) where
    show (Vote w cs) = showFFloat (Just 4) (fromRational w :: Double) $ " " ++ show cs

data VoteCount a = VoteCount {candidate :: a, count :: Rational}

instance (Show a) => Show (VoteCount a) where
    show (VoteCount c cnt) = show c ++ " " ++ showFFloat (Just 4) (fromRational cnt :: Double) ""

countVotes :: (Ord a) => S.Set a -> [Vote a] -> [VoteCount a]
countVotes candidates = reverse . map (uncurry VoteCount) . sortOn snd . M.toList . foldl (flip . uncurry $ M.insertWith (+)) x . map f
    where f v = (NE.head . choices $ v, weight v)
          x = M.fromSet (const 0) candidates

splitTier :: [VoteCount a] -> Maybe Rational -> [[a]]
splitTier (x:xs) Nothing = let y:ys = splitTier xs (Just (count x)) in (candidate x:y):ys
splitTier (x:xs) (Just c)
    | count x == c = let y:ys = splitTier xs (Just c) in (candidate x:y):ys
    | otherwise = [] : splitTier (x:xs) Nothing
splitTier [] Nothing = []
splitTier [] _ = [[]]

elect :: (Ord a) => Integer -> [VoteCount a] -> [Vote a] -> [Vote a]
elect quota vcs votes = transferImpl (M.fromList . map vc2cm $ elected) votes
    where elected = filter ((>= toRational quota) . count) vcs
          vc2cm vc = (candidate vc, min (exv / xfc) 1)
              where xfc = sumOn' weight . filter (xffrm $ candidate vc) $ votes
                    exv = count vc - (quota % 1)
          xffrm c v = let cs = choices v in (NE.head cs == c) && any (`notElem` (candidate <$> elected)) cs

eliminate :: (Ord a) => [a] -> [Vote a] -> [Vote a]
eliminate candidates = transferImpl (M.fromList . map (, 1 % 1) $ candidates)

transferImpl :: (Ord a) => M.Map a Rational -> [Vote a] -> [Vote a]
transferImpl candidates = map recon . filter valid . map trim
    where trim v = (weight v * ratio v, NE.filter (`M.notMember` candidates) . choices $ v)
          ratio v = M.findWithDefault (1 % 1) (NE.head . choices $ v) candidates
          valid (_, []) = False
          valid (w, _) = w /= 0
          recon (w, c) = Vote w (NE.fromList c)

{-# LANGUAGE TupleSections #-}

module Lib
    ( Vote (..)
    , VoteCount (..)
    , fromChoices
    , countVotes
    , elect
    , eliminate
    ) where

import Data.List (sortOn)
import Data.List.Extra (sumOn')
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.Ratio

data Vote a = Vote {weight :: Rational, choices :: NE.NonEmpty a}

data VoteCount a = VoteCount {candidate :: a, count :: Rational}

fromChoices :: [a] -> Vote a
fromChoices = Vote (1 % 1) . NE.fromList

countVotes :: (Ord a) => [Vote a] -> [VoteCount a]
countVotes = map (uncurry VoteCount) . sortOn snd . M.toList . M.fromListWith (+) . map f
    where f v = (NE.head . choices $ v, weight v)

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

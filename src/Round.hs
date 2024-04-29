module Round
    ( Env (..)
    , STV (..)
    , makeEnv
    , fromChoices
    , STVResult (..)
    , run
    , ncandidates
    ) where

import Core
import Control.Monad (when, unless)
import Data.List.Extra (sumOn')
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Numeric (showFFloat)
import Data.Ratio

data Env = Env { quota :: Integer
               , npos :: Int
               , quiet :: Bool
               , verbose :: Bool
               } deriving (Show)

makeEnv :: STV a -> Int -> Bool -> Bool -> Env
makeEnv stv npos = Env quota npos
    where nvotes = length (votes stv)
          quota = toInteger $ nvotes `div` (npos + 1) + 1

data STV a = STV { candidates :: S.Set a
                 , votes :: [Vote a]
                 , elected :: [[[a]]]
                 , eliminated :: [[a]]
                 , roundNumber :: Int
                 , coeff :: Rational
                 } deriving (Show)

data STVResult a =
    NoChaos [[[a]]] [[a]]
    | MinorChaos [[a]] [([[a]], Rational)]
    | MajorChaos [([[a]], Rational)]

run :: (Show a, Ord a) => Env -> STV a -> IO (STVResult a)
run env stv = do
    let Env quota npos quiet _ = env
    unless quiet . putStrLn $ show (length (votes stv)) ++ " votes, " ++ show (ncandidates stv) ++ " candidates, " ++ show npos ++ " positions, Droop quota = " ++ show quota
    final <- rounds env stv
    return (case final of
           [stv] -> NoChaos (reverse $ elected stv) (eliminated stv)
           _ -> case combine (concat . reverse . elected) final of
                     [(el, _)] -> MinorChaos el (combine eliminated final)
                     cf -> MajorChaos cf)

ncandidates :: STV a -> Int
ncandidates = S.size . candidates

fromChoices :: (Ord a) => [[a]] -> STV a
fromChoices l = STV (S.fromList . concat $ l) (map f l) [] [] 0 1
    where f = Vote (1 % 1) . NE.fromList

stvRound :: (Show a, Ord a) => Env -> STV a -> IO [STV a]
stvRound env (STV candidates votes elected eliminated rnd coeff) = do
    let Env quota npos quiet verbose = env
    let tvv = (fromRational $ sumOn' weight votes) :: Double
    let elcnt = length . concat . concat $ elected
    let excnt = length . concat $ eliminated
    unless quiet . putStrLn $ "[[Round " ++ show rnd ++ "]]: " ++ show (S.size candidates) ++ "/" ++ show elcnt ++ "/" ++ show excnt ++ " candidates, " ++ showFFloat (Just 4) tvv " valid votes"
    when verbose $ print votes
    let vcs = countVotes candidates votes
    when verbose $ print vcs
    let elc = filter ((>= toRational quota) . count) vcs
    let worst = count (last vcs)
    let exc = filter ((== worst) . count) vcs
    let ex = candidate <$> exc
    if not . null $ elc then do
        unless quiet . putStrLn $ show (length elc) ++ " candidate(s) elected:"
        when verbose $ mapM_ print elc
        let el = candidate <$> elc
        return [STV (candidates S.\\ S.fromList el) (elect quota vcs votes) (splitTier elc Nothing : elected) eliminated (rnd + 1) coeff]
    else if length ex == 1 || npos - elcnt <= S.size candidates - length ex then do
        unless quiet . putStrLn $ show (length exc) ++ " candidate(s) eliminated:"
        when verbose $ mapM_ print exc
        return [STV (candidates S.\\ S.fromList ex) (eliminate ex votes) elected (ex : eliminated) (rnd + 1) coeff]
    else do
        unless quiet . putStrLn $ show (length ex) ++ " candidates on tier for elimination at " ++ show worst ++ ":"
        when verbose $ mapM_ print ex
        concat <$> mapM (\x -> do
            unless quiet . putStrLn $ "Split: " ++ show x
            let co = (coeff /) . toRational $ length ex
            rounds env (STV (S.delete x candidates) (eliminate [x] votes) elected ([x] : eliminated) (rnd + 1) co)) ex

rounds :: (Show a, Ord a) => Env -> STV a -> IO [STV a]
rounds env stv = do
    let Env _ npos quiet verbose = env
    let elcnt = length . concat . concat . elected $ stv
    if elcnt > npos then do
        putStrLn "Warning: too many candidates elected!"
        return [stv]
    else if elcnt == npos then do
        unless quiet $ putStrLn "Election completed because all positions filled."
        return [stv]
    else if null $ votes stv then do
        unless quiet $ putStrLn "Election completed because no vote left."
        return [stv]
    else if elcnt + ncandidates stv == npos then do
        unless quiet $ putStrLn "Election assumed to complete."
        let STV cs vs el ex rnd co = stv
        when verbose $ print vs
        let vcs = countVotes cs vs
        when verbose $ print vcs
        return [STV S.empty [] (splitTier vcs Nothing : el) ex rnd co]
    else if elcnt + ncandidates stv < npos then do
        putStrLn "Warning: not enough candidates elected!"
        return [stv]
    else do
        stvs <- stvRound env stv
        concat <$> mapM (rounds env) stvs

combine :: (Ord a) => (STV a -> [[a]]) -> [STV a] -> [([[a]], Rational)]
combine selector = M.toList . foldl f M.empty
    where f m stv = M.insertWith (+) (selector stv) (coeff stv) m

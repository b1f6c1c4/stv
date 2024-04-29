module Main (main) where

import Lib
import Control.Monad (when, unless)
import Data.List.Extra (sumOn')
import qualified Data.Set as S
import qualified Data.Map as M
import System.IO (Handle, hGetContents, IOMode (ReadMode), withFile)
import Data.List.Split (wordsBy)
import System.Environment (getArgs)
import Numeric (showFFloat)

data Env a = Env { quota :: Integer
                 , npos :: Int
                 , quiet :: Bool
                 , verbose :: Bool
                 } deriving (Show)

data STV a = STV { candidates :: S.Set a
                 , votes :: [Vote a]
                 , elected :: [[[a]]]
                 , eliminated :: [[a]]
                 , roundNumber :: Int
                 , coeff :: Rational
                 } deriving (Show)

stvRound :: (Show a, Ord a) => Env a -> STV a -> IO [STV a]
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

rounds :: (Show a, Ord a) => Env a -> STV a -> IO [STV a]
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
    else if elcnt + S.size (candidates stv) == npos then do
        unless quiet $ putStrLn "Election assumed to complete."
        let STV cs vs el ex rnd co = stv
        when verbose $ print vs
        let vcs = countVotes cs vs
        when verbose $ print vcs
        return [STV S.empty [] (splitTier vcs Nothing : el) ex rnd co]
    else if elcnt + S.size (candidates stv) < npos then do
        putStrLn "Warning: not enough candidates elected!"
        return [stv]
    else do
        stvs <- stvRound env stv
        concat <$> mapM (rounds env) stvs

combine :: (Ord a) => (STV a -> [[a]]) -> [STV a] -> [([[a]], Rational)]
combine selector = M.toList . foldl f M.empty
    where f m stv = M.insertWith (+) (selector stv) (coeff stv) m

parseLine :: String -> [String]
parseLine = cleanUp . wordsBy (`elem` ", ")
    where cleanUp (x:xs) = x : cleanUp (filter (/= x) xs)
          cleanUp [] = []

fromFile :: Handle -> IO (STV String)
fromFile h = do
    l <- filter (not . null) . map parseLine . lines <$> hGetContents h
    seq (length l) (return ()) -- dark magic, don't touch
    return (STV (S.fromList . concat $ l) (map fromChoices l) [] [] 0 1)

main :: IO ()
main = do
    args <- getArgs
    stv <- withFile (head args) ReadMode fromFile
    let npos = read (args !! 1) :: Int
    let nvotes = length (votes stv)
    let quota = toInteger $ nvotes `div` (npos + 1) + 1
    let quiet = True
    let env = Env quota npos quiet False
    unless quiet . putStrLn $ show nvotes ++ " votes, " ++ show (S.size . candidates $ stv) ++ " candidates, " ++ show npos ++ " positions, Droop quota = " ++ show quota
    final <- rounds env stv
    case final of
         [stv] -> do
             putStrLn "Candidates Elected: (from best to better)"
             mapM_ print . reverse . elected $ stv
             putStrLn "Candidates Eliminated: (from worse to worst)"
             mapM_ print . eliminated $ stv
         _ -> case combine (concat . reverse . elected) final of
                   [(el, _)] -> do
                       putStrLn "Candidates Elected: (from best to better)"
                       print el
                       putStrLn "Candidates Eliminated: (from worse to worst) and the configuration ratio:"
                       mapM_ (\(k, v) -> putStrLn $ show k ++ "(" ++ showFFloat (Just 4) (fromRational (100 * v) :: Double) "%)") $ combine eliminated final
                   cfinal -> do
                       putStrLn "Candidates Elected (from best to better) and the configuration ratio:"
                       mapM_ (\(k, v) -> putStrLn $ show k ++ "(" ++ showFFloat (Just 4) (fromRational (100 * v) :: Double) "%)") cfinal

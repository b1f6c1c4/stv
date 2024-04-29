module Main (main) where

import Lib
import Control.Monad (when)
import Data.List.Extra (sumOn')
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
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
                 } deriving (Show)

stvRound :: (Show a, Ord a) => ([a] -> IO [a]) -> Env a -> STV a -> IO (STV a)
stvRound selector (Env quota npos quiet verbose) (STV candidates votes elected eliminated rnd) = do
    let tvv = (fromRational $ sumOn' weight votes) :: Double
    let elcnt = length . concat . concat $ elected
    let excnt = length . concat $ eliminated
    when (not quiet) . putStrLn $ "[[Round " ++ show rnd ++ "]]: " ++ show (S.size candidates) ++ "/" ++ show elcnt ++ "/" ++ show excnt ++ " candidates, " ++ showFFloat (Just 4) tvv " valid votes"
    let vcs = countVotes candidates votes
    when verbose $ print candidates
    when verbose $ print vcs
    let elc = filter ((>= toRational quota) . count) vcs
    let worst = count (last vcs)
    let exc = filter ((== worst) . count) vcs
    let ex = candidate <$> exc
    if not . null $ elc then do
        when (not quiet) . putStrLn $ show (length elc) ++ " candidate(s) elected:"
        when verbose $ mapM_ print elc
        let el = candidate <$> elc
        return (STV (candidates S.\\ S.fromList el) (elect quota vcs votes) (splitTier elc Nothing : elected) eliminated (rnd + 1))
    else if length ex == 1 || npos - elcnt <= S.size candidates - length ex then do
        when (not quiet) . putStrLn $ show (length exc) ++ " candidate(s) eliminated:"
        when verbose $ mapM_ print exc
        return (STV (candidates S.\\ S.fromList ex) (eliminate ex votes) elected (ex : eliminated) (rnd + 1))
    else do
        when (not quiet) . putStrLn $ show (length ex) ++ " candidates on tier for elimination at " ++ show worst ++ ":"
        when verbose $ mapM_ print ex
        exs <- selector ex
        when (not quiet) . putStrLn $ show (length exs) ++ " candidate(s) chosed:"
        when verbose $ mapM_ print exs
        return (STV (candidates S.\\ S.fromList exs) (eliminate exs votes) elected (exs : eliminated) (rnd + 1))

rounds :: (Show a, Ord a) => ([a] -> IO [a]) -> Env a -> STV a -> IO (STV a)
rounds selector (Env quota npos quiet verbose) stv = do
    let elcnt = length . concat . concat . elected $ stv
    if elcnt > npos then do
        putStrLn "Internal Error: too many candidates elected!"
        return stv
    else if elcnt == npos then do
        when (not quiet) $ putStrLn "Election completed."
        return stv
    else if elcnt + S.size (candidates stv) == npos then do
        when (not quiet) $ putStrLn "Election assumed to complete."
        let el = S.toList . S.fromList . concatMap (NE.toList . choices) . votes $ stv
        return (STV S.empty [] ([el] : elected stv) (eliminated stv) (roundNumber stv))
    else if elcnt + S.size (candidates stv) < npos then do
        putStrLn "Error: not enough candidates elected!"
        return stv
    else do
        rounds selector (Env quota npos quiet verbose) =<< stvRound selector (Env quota npos quiet verbose) stv

parseLine :: String -> [String]
parseLine = cleanUp . wordsBy (`elem` ", ")
    where cleanUp (x:xs) = x : cleanUp (filter (/= x) xs)
          cleanUp [] = []

fromFile :: Handle -> IO (STV String)
fromFile h = do
    l <- filter (not . null) . map parseLine . lines <$> hGetContents h
    seq (length l) (return ()) -- dark magic, don't touch
    return (STV (S.fromList . concat $ l) (map fromChoices l) [] [] 0)

manualSelector :: [String] -> IO [String]
manualSelector choices = do
    putStrLn "Which one(s) to eliminate?"
    sel <- parseLine <$> getLine
    if not (null sel) && all (`elem` choices) sel
       then return sel
       else do
           putStrLn "Invalid selection, try again."
           manualSelector choices

main :: IO ()
main = do
    args <- getArgs
    stv <- withFile (head args) ReadMode fromFile
    let npos = read (args !! 1) :: Int
    let nvotes = length (votes stv)
    let quota = toInteger $ nvotes `div` (npos + 1) + 1
    let quiet = False
    let env = Env quota npos quiet False
    when (not quiet) . putStrLn $ show nvotes ++ " votes, " ++ show (S.size . candidates $ stv) ++ " candidates, " ++ show npos ++ " positions, Droop quota = " ++ show quota
    final <- rounds eliminationSelector env stv
    when (not quiet) $ putStrLn "Candidates Elected: (from best to better)"
    mapM_ print . reverse . elected $ final
    when (not quiet) $ putStrLn "Candidates Eliminated: (from worse to worst)"
    mapM_ print . eliminated $ final

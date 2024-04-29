module Main (main) where

import Lib
import Data.List.Extra (sumOn')
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import System.IO (Handle, hGetContents, IOMode (ReadMode), withFile)
import Data.List.Split (wordsBy, split, whenElt)
import System.Environment (getArgs)

data STV a = STV { ncandidates :: Int
                 , votes :: [Vote a]
                 , elected :: [[[a]]]
                 , eliminated :: [[a]]
                 , roundNumber :: Int
                 } deriving (Show)

stvRound :: (Show a, Ord a) => ([a] -> IO [a]) -> Integer -> STV a -> IO (STV a)
stvRound selector quota (STV nc votes elected eliminated rnd) = do
    putStrLn $ "Round " ++ show rnd ++ ": " ++ show nc ++ " candidates, " ++ show (sumOn' weight votes) ++ " valid votes"
    let vcs = countVotes votes
    let elc = filter ((>= toRational quota) . count) vcs
    let worst = count (last vcs)
    let exc = filter ((== worst) . count) vcs
    let ex = candidate <$> exc
    if not . null $ elc then do
        putStrLn $ show (length elc) ++ " candidate(s) elected:"
        mapM_ print elc
        return (STV (nc - length elc) (elect quota vcs votes) (splitTier elc Nothing : elected) eliminated (rnd + 1))
    else if length ex == 1 then do
        putStrLn $ "Eliminated: " ++ show (head exc)
        return (STV (nc - 1) (eliminate ex votes) elected (ex : eliminated) (rnd + 1))
    else do
        putStrLn $ show (length ex) ++ " candidates on tier for elimination at " ++ show worst ++ ":"
        mapM_ print ex
        exs <- selector ex
        putStrLn $ show (length exs) ++ " candidate(s) chosed:"
        mapM_ print exs
        return (STV (nc - length exs) (eliminate exs votes) elected (exs : eliminated) (rnd + 1))

rounds :: (Show a, Ord a) => ([a] -> IO [a]) -> Integer -> Int -> STV a -> IO (STV a)
rounds selector quota npos stv = do
    let elc = length . concat . concat . elected $ stv
    if elc > npos then do
        putStrLn "Internal Error: too many candidates elected!"
        return stv
    else if elc == npos then do
        putStrLn "Election completed."
        return stv
    else if elc + ncandidates stv == npos then do
        putStrLn "Election assumed to complete."
        let el = S.toList . S.fromList . concatMap (NE.toList . choices) . votes $ stv
        return (STV 0 [] ([el] : elected stv) (eliminated stv) (roundNumber stv))
    else do
        rounds selector quota npos =<< stvRound selector quota stv

parseLine :: String -> [String]
parseLine = cleanUp . wordsBy (`elem` ", ")
    where cleanUp (x:xs) = x : cleanUp (filter (/= x) xs)
          cleanUp [] = []

fromFile :: Handle -> IO (STV String)
fromFile h = do
    l <- filter (not . null) . map parseLine . lines <$> hGetContents h
    seq (length l) (return ()) -- dark magic, don't touch
    let nc = S.size . S.fromList . concat $ l
    return (STV nc (map fromChoices l) [] [] 0)

eliminationSelector :: [String] -> IO [String]
eliminationSelector choices = do
    putStrLn "Which one(s) to eliminate?"
    sel <- parseLine <$> getLine
    if not (null sel) && all (`elem` choices) sel
       then return sel
       else do
           putStrLn "Invalid selection, try again."
           eliminationSelector choices

main :: IO ()
main = do
    args <- getArgs
    stv <- withFile (head args) ReadMode fromFile
    let npos = read (args !! 1) :: Int
    let nvotes = length (votes stv)
    let quota = toInteger $ nvotes `div` (npos + 1) + 1
    putStrLn $ show nvotes ++ " votes, " ++ show (ncandidates stv) ++ " candidates, " ++ show npos ++ " positions, Droop quota = " ++ show quota
    final <- rounds eliminationSelector quota npos stv
    putStrLn "Candidates Elected: (from best to better)"
    mapM_ print . reverse . elected $ final
    putStrLn "Candidates Eliminated: (from worse to worst)"
    mapM_ print . eliminated $ final

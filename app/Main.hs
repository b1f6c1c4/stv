module Main (main) where

import Round
import System.Environment (getArgs, getProgName)
import System.IO (hGetContents, IOMode (ReadMode), withFile)
import Data.List.Split (wordsBy)
import Numeric (showFFloat)
import Data.List (intercalate)

parseLine :: String -> [String]
parseLine = cleanUp . wordsBy (`elem` ", \t")
    where cleanUp (x:xs) = x : cleanUp (filter (/= x) xs)
          cleanUp [] = []

main :: IO ()
main = do
    args <- getArgs
    if null args
       then do
           nm <- getProgName
           fail $ "Usage: " ++ nm ++ " <csv-file> [<npos>]"
       else do
           stv <- withFile (head args) ReadMode (\h -> do
               l <- filter (not . null) . map parseLine . lines <$> hGetContents h
               seq (length l) (return ()) -- dark magic, don't touch
               return (fromChoices l))
           if length args >= 2
              then let np = read (args !! 1) in eval stv False True np
              else mapM_ (eval stv True False) [1 .. ncandidates stv]

eval :: STV String -> Bool -> Bool -> Int -> IO ()
eval stv quiet verbose npos = do
    result <- run (makeEnv stv npos quiet verbose) stv
    case result of
         NoChaos el ex -> do
             putStr . intercalate " > " . map (intercalate "=") $ concat el
             if null ex
                then putStrLn ""
                else do
                    putStr "  >>  "
                    putStrLn . intercalate " > " . map (intercalate "=") $ ex
         MinorChaos el exs -> do
             putStr . intercalate " > " . map (intercalate "=") $ el
             putStr "  >>  (the rest:"
             mapM_ (\(k, v) -> do
                   putStrLn ""
                   putStr "            "
                   putStr $ "[" ++ showFFloat (Just 4) (fromRational (100 * v) :: Double) "%] "
                   putStr . intercalate " > " . map (intercalate "=") $ k) exs
             putStrLn ")"
         MajorChaos els -> do
             mapM_ (\(k, v) -> do
                   putStr $ "[" ++ showFFloat (Just 4) (fromRational (100 * v) :: Double) "%] "
                   putStr . intercalate " > " . map (intercalate "=") $ k
                   putStrLn "  >>  (the rest) *") els

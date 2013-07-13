module Main 
where

import Npm( doGetNpm )
import System.Environment

parseArgs :: [String] -> (String, String)
parseArgs xs
    | length xs == 2 = ((head xs), (head $ tail xs))
    | otherwise = error "Error in parameters" 

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show $ parseArgs args

module Main 
where

import Npm( doGetNpm, makeEbuildS, showNpmEbuild )
import Control.Monad
import System.Environment
import System.Console.GetOpt

-- Flag type for Options
data Flag = NoDeps 
        | PkgName String
        | PkgVersion String
        | Overlay String 
        deriving (Show, Eq)

-- Options type. 
data Options = Options { optNoDeps :: Bool
                , optOverlay :: String
                , optPkgName :: String
                , optPkgVersion :: String }
                deriving Show

-- Default options
startOptions = Options { optNoDeps = False
                , optOverlay = "" 
                , optPkgName = ""
                , optPkgVersion = "" }
                

-- Options.
options :: [ OptDescr (Options -> IO Options) ]
options = [ Option ['N'] ["no-deps"] (NoArg (\opt -> return opt {optNoDeps = True})) "Don't create ebuilds for dependencies.",
    Option ['o'] ["overlay"] (ReqArg (\arg opt -> return opt {optOverlay = arg}) "DIR") "Path to the overlay where ebuils are to be written to.",
    Option ['n'] ["pkg-name"] (ReqArg (\arg opt -> return opt {optPkgName = arg}) "NAME") "Name of the Npm package to build.",
    Option ['v'] ["pkg-version"] (ReqArg (\arg opt -> return opt {optPkgVersion = arg}) "VER") "Version of the Npm package to build."]

-- parseArgs
-- Applies getOpt to the command line arguments.
parseArgs :: [String] -> IO ([Options -> IO Options], [String])
parseArgs args =
    case getOpt RequireOrder options args of
        (o,n,[]  ) -> return (o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: g-npm [OPTION...]"

-- Main
main :: IO ()
main = do
    args <- getArgs
    (opts, non_opts) <- parseArgs args
    opts' <- foldl (>>=) (return startOptions) opts

    pkg <- doGetNpm (optPkgName opts') (optPkgVersion opts')

    case pkg of
        Nothing -> ioError (userError ("Package " ++ (optPkgName opts') ++ 
            " not found."))
        Just pkg -> putStrLn $ (showNpmEbuild pkg)


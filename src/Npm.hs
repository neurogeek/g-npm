{-- This module is used to create Npm data types from 
 -  JSON descriptions, with queries made to the NPM Registry
 -  (http://registry.npmjs.org) 
 -  Author: Jesus Rivero <neurogeek@gentoo.org> --}

module Npm
    ( doGetNpm,
      makeEbuildS,
      showNpmEbuild,
      Npm )
where

import Text.JSON
import Network.HTTP
import Data.List

-- Test Data
jsStr :: String
jsStr = "{\"name\":\"grunt-cli\",\"description\":\"The grunt command line interface.\",\"version\":\"0.1.8\",\"author\":{\"name\":\"Grunt Team\"},\"homepage\":\"http://gruntjs.com/\",\"repository\":{\"type\":\"git\",\"url\":\"git://github.com/gruntjs/grunt-cli.git\"},\"bugs\":{\"url\":\"http://github.com/gruntjs/grunt-cli/issues\"},\"licenses\":[{\"type\":\"MIT\",\"url\":\"http://github.com/gruntjs/grunt-cli/blob/master/LICENSE-MIT\"}],\"bin\":{\"grunt\":\"bin/grunt\"},\"engines\":{\"node\":\">= 0.8.0\"},\"scripts\":{\"test\":\"node bin/grunt test\"},\"preferGlobal\":true,\"dependencies\":{\"nopt\":\"~1.0.10\",\"findup-sync\":\"~0.1.0\",\"resolve\":\"~0.3.1\"},\"devDependencies\":{\"grunt\":\"~0.4.0\",\"grunt-contrib-jshint\":\"~0.2.0\"},\"contributors\":[{\"name\":\"Tyler Kellen\",\"url\":\"http://goingslowly.com\"},{\"name\":\"Ben Alman\",\"url\":\"http://gruntjs.com\"},{\"name\":\"Scott González\",\"url\":\"http://nemikor.com\"},{\"name\":\"Forbes Lindesay\",\"url\":\"https://github.com/\"}],\"readmeFilename\":\"README.md\",\"_id\":\"grunt-cli@0.1.8\",\"dist\":{\"shasum\":\"74b59b91487a4ce061a4001d592ddac85de402d2\",\"tarball\":\"http://registry.npmjs.org/grunt-cli/-/grunt-cli-0.1.8.tgz\"},\"_from\":\".\",\"_npmVersion\":\"1.2.15\",\"_npmUser\":{\"name\":\"tkellen\",\"email\":\"tyler@sleekcode.net\"},\"maintainers\":[{\"name\":\"cowboy\",\"email\":\"cowboy@rj3.net\"},{\"name\":\"tkellen\",\"email\":\"tyler@sleekcode.net\"}],\"directories\":{}}"

-- Npm data. Represents a package.js from 
-- an Npm package
data Npm = Npm {
        name :: String,
        version :: String,
        description :: String,
        dependencies :: [(String, String)] }
        deriving Show

-- getNpmUrl - Returns the URL to query the NPM registry.
getNpmUrl :: String -> String -> String
getNpmUrl p v = "http://registry.npmjs.org/" ++ p ++ "/" ++ v

-- Makes the HTTP request to the NPM Registry
doGetNpm :: String -> String -> IO (Maybe Npm)
doGetNpm p v = simpleHTTP (getRequest $ getNpmUrl p v)
                    >>= fmap makeNpm . getResponseBody

{--
doGetNpmDeps :: IO (Maybe Npm) -> IO ([IO (Maybe Npm)])
doGetNpmDeps f = fmap doGetNpmDeps' f
    where
        doGetNpmDeps' :: Maybe Npm -> [IO (Maybe Npm)]
        doGetNpmDeps' n = map (\(p, v) -> doGetNpm p v) $ extractDeps n 

        extractDeps :: Maybe Npm -> [(String, String)]
        extractDeps (Just n) = dependencies n
        extractDeps Nothing = [("", "")]

--}
--
{--
makeEbuild :: IO (Maybe Npm) -> IO (String)
makeEbuild n = fmap doEbuild n
    where
        doEbuild :: Maybe Npm -> String
        doEbuild (Just n) = show $ (name n) ++ (version n)
        doEbuild Nothing = ""
--}
makeEbuildS :: Maybe Npm -> String
makeEbuildS n = doEbuildS n
    where
        doEbuildS :: Maybe Npm -> String
        doEbuildS (Just n) = showNpmEbuild n
        doEbuildS Nothing = ""

showNpmEbuild :: Npm -> String
showNpmEbuild n = "# Copyright 1999-2013 Gentoo Foundation" ++ "\n" ++
                  "# Distributed under the terms of the GNU General Public License v2" ++ "\n" ++
                  "# $Header: $" ++ "\n" ++ 
                  "\n" ++
                  "EAPI=5" ++ "\n" ++
                  "\n" ++
                  "inherit multilib npm" ++ "\n" ++
                  "\n" ++
                  "DESCRIPTION=\"" ++ description n ++ "\"\n" ++
                  "\n" ++
                  "LICENSE=\"MIT\"" ++ "\n" ++
                  "SLOT=\"0\"" ++ "\n" ++
                  "KEYWORDS=\"~amd64 ~x86\"" ++ "\n" ++
                  "IUSE=\"\"" ++ "\n" ++
                  "DEPEND=\">=net-libs/nodejs-0.8.10\"" ++ "\n" ++
                  "RDEPEND=\"" ++ depStrings n ++
                  "\n\t${DEPEND}\"" ++ "\n"

depStrings :: Npm -> String
depStrings pkg = concat $ 
                    intersperse "\n\t" $ 
                        map (\(x, y) -> ">=dev-nodejs/" ++ x ++ "-" ++ (cleanVersion y)) $ dependencies pkg
                where
                    cleanVersion :: String -> String
                    cleanVersion n 
                            | head n == '~' = tail n
                            | otherwise = n

--
-- Converts the resulting JSON from querying the NPM Registry,
-- to actual Npm data.
makeNpm :: String -> Maybe Npm
makeNpm s = makeNpm' $ decode s
    where
        makeNpm':: Result (JSObject JSValue) -> Maybe Npm
        makeNpm' (Ok x) = let a = (buildNpm x) in 
                                if (name a) == "" then Nothing
                                else Just a
        makeNpm' _ = Nothing

        getString (Ok s) = fromJSString s
        getString _ = ""

        getMaybeString (Ok s) = Just $ fromJSString s
        getMaybeString _ = Nothing

        getDepends :: Result (JSObject JSValue) -> [(String, String)]
        getDepends (Ok s) = processDep $ fromJSObject s
        getDepends _ = []

        processDep :: [(String, JSValue)] -> [(String, String)]
        processDep [] = []
        processDep (x:xs) = [(fst x, (\(JSString s) -> fromJSString s) $ snd x)] ++ processDep xs

        buildNpm x = Npm {
            name = getString $ valFromObj "name" x,
            version = getString $ valFromObj "version" x,
            description = getString $ valFromObj "description" x,
            dependencies = getDepends $ valFromObj "dependencies" x }


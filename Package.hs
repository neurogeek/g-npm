{-- This module is used to create Npm data types from 
 -  JSON descriptions, with queries made to the NPM Registry
 -  (http://registry.npmjs.org 
 -  Author: Jesus Rivero <neurogeek@gentoo.org> --}

module Package
where

import Text.JSON
import Network.HTTP

-- Npm dependencies
data NpmDep = NpmDep (String, String)
-- Npm data. Represents a package.js from 
-- an Npm package
data Npm = Npm {
        name :: Maybe String,
        version :: Maybe String,
        description :: Maybe String,
        dependencies :: Maybe [String] }
        --dependencies :: Maybe NpmDep }

-- getNpmUrl - Returns the URL to query the NPM registry.
getNpmUrl :: String -> String -> String
getNpmUrl p v = "http://registry.npmjs.org/" ++ p ++ "/" ++ v

-- Makes the HTTP request to the NPM Registry
doPackage :: String -> String -> IO (Maybe Npm)
doPackage p v = simpleHTTP (getRequest $ getNpmUrl p v) 
                    >>= fmap toJSON . getResponseBody

-- Converts the resulting JSON from querying the NPM Registry,
-- to actual Npm data.
toJSON :: String -> Maybe Npm
toJSON s = toJSON' $ decode s
    where
        toJSON':: Result (JSObject JSValue) -> Maybe Npm
        toJSON' (Ok x) = Just (buildNpm x)
        toJSON' _ = Nothing

        getString (Ok s) = Just $ fromJSString s
        getString _ = Nothing

        getDepends :: Result (JSObject JSValue) -> Maybe [String]
        getDepends (Ok s) = Just $ map (\(x, y) -> x) $ fromJSObject s 
        getDepends _ = Nothing

        buildNpm x = Npm {
            name = getString $ valFromObj "name" x,
            version = getString $ valFromObj "version" x,
            description = getString $ valFromObj "description" x,
            dependencies = getDepends $ valFromObj "dependencies" x }

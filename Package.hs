{-- This module is used to create Npm data types from 
 -  JSON descriptions, with queries made to the NPM Registry
 -  (http://registry.npmjs.org 
 -  Author: Jesus Rivero <neurogeek@gentoo.org> --}

module Package
where

import Text.JSON
import Network.HTTP

-- Test Data
jsStr :: String
jsStr = "{\"name\":\"grunt-cli\",\"description\":\"The grunt command line interface.\",\"version\":\"0.1.8\",\"author\":{\"name\":\"Grunt Team\"},\"homepage\":\"http://gruntjs.com/\",\"repository\":{\"type\":\"git\",\"url\":\"git://github.com/gruntjs/grunt-cli.git\"},\"bugs\":{\"url\":\"http://github.com/gruntjs/grunt-cli/issues\"},\"licenses\":[{\"type\":\"MIT\",\"url\":\"http://github.com/gruntjs/grunt-cli/blob/master/LICENSE-MIT\"}],\"bin\":{\"grunt\":\"bin/grunt\"},\"engines\":{\"node\":\">= 0.8.0\"},\"scripts\":{\"test\":\"node bin/grunt test\"},\"preferGlobal\":true,\"dependencies\":{\"nopt\":\"~1.0.10\",\"findup-sync\":\"~0.1.0\",\"resolve\":\"~0.3.1\"},\"devDependencies\":{\"grunt\":\"~0.4.0\",\"grunt-contrib-jshint\":\"~0.2.0\"},\"contributors\":[{\"name\":\"Tyler Kellen\",\"url\":\"http://goingslowly.com\"},{\"name\":\"Ben Alman\",\"url\":\"http://gruntjs.com\"},{\"name\":\"Scott González\",\"url\":\"http://nemikor.com\"},{\"name\":\"Forbes Lindesay\",\"url\":\"https://github.com/\"}],\"readmeFilename\":\"README.md\",\"_id\":\"grunt-cli@0.1.8\",\"dist\":{\"shasum\":\"74b59b91487a4ce061a4001d592ddac85de402d2\",\"tarball\":\"http://registry.npmjs.org/grunt-cli/-/grunt-cli-0.1.8.tgz\"},\"_from\":\".\",\"_npmVersion\":\"1.2.15\",\"_npmUser\":{\"name\":\"tkellen\",\"email\":\"tyler@sleekcode.net\"},\"maintainers\":[{\"name\":\"cowboy\",\"email\":\"cowboy@rj3.net\"},{\"name\":\"tkellen\",\"email\":\"tyler@sleekcode.net\"}],\"directories\":{}}"

-- Npm dependencies
data NpmDep = NpmDep (String, String)
-- Npm data. Represents a package.js from 
-- an Npm package
data Npm = Npm {
        name :: Maybe String,
        version :: Maybe String,
        description :: Maybe String,
        dependencies :: [(String, String)] }
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

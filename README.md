g-npm
=====

Tool to create ebuild from npm packages.
*Update: The g-npm project now has limited functionality. It is now possible to create an ebuild using the tool. Still much work is required.*

Installing
----------

To install, you can use Cabal:

$ cabal install

or 

$ runhaskell Setup.hs install

Usage
-----

To create an ebuild (e.g. grunt-cli-0.1.8):

$ g-npm -n grunt-cli -v 0.1.8 > grunt-cli-0.1.8.ebuild

TODO
----

- Actually write a file instead of outputting to stdout.
- Read overlay from command-line (-o option).
- Activate ebuild creation of dependencies.
- More to come

# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\ b -> fmap (\ g -> show (b g))`

This might seem similar to what [djinn](https://hackage.haskell.org/package/djinn)
does, but there are significant differences.

## Documentation

There is [an introductory paper/report](https://github.com/lspitzner/exference/exference.pdf)
which describes the exact features of the tool, and contrasts to existing projects.

## Repositories

Currently available are two cabal packages, living in their own repositories:
- exference-exference-core: core functionality library
- exference-exference: both library with advanced interface and executable

Note that I have not published the irc bot package (yet).

## Known issues

- The naming of the executable is misleading;
  the tests should be put in a proper test-suite.
- Type synonyms are not supported yet.
- Kinds are not checked (which can be seen as both advantage and disadvantage, see paper).
- Pattern-matching on multi-constructor data-types is not supported - theoretically,
  this is possible and i have implemented something in this direction recently, but
  the performance is horrible without further optimizations.

## Compiling from source

Compiling the executable involves something along the lines of

~~~~
cabal sandbox init
cabal sandbox add-source exference-core
cabal install --deps
cabal configure -fbuild-executable
cabal build
# and, for example
cabal run -- "(Show b) => (a->b) -> [a] -> [String]"
~~~~


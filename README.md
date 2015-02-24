# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\ b -> fmap (\ g -> show (b g))`

This might seem similar to what [djinn](https://hackage.haskell.org/package/djinn)
does, but there are significant differences.

## Documentation

There is [an introductory paper/report](https://github.com/lspitzner/exference/raw/master/exference.pdf)
which describes the exact features of the tool, and contrasts to existing projects.

I try to keep a bot running on irc (`#exference`) so you can play around
with no need to install locally. But it is kinda old hardware.
It reacts to `:exf` prefix, i.e. `:exf Monad m => m (m a) -> m a`.

## Known issues

  (note that this is just an addition to the detailed feature description in the report)

- The executable is badly named (`test-main`);
  the tests should be put in a proper test-suite.
  (initially the executable was created for testing purposes
  , but now serves as command-line interface;
  this is why no parameters run tests.)
- Type synonyms are not supported yet.
- Kinds are not checked (which can be seen as both advantage and disadvantage, see report).
- Pattern-matching on multi-constructor data-types is not supported - theoretically,
  this is possible and i have implemented something in this direction recently, but
  the performance is horrible without further optimizations.
- The dependency bounds of the cabal packages should be cleaned up/checked.
  I postponed this as there is no automated way to do this. stupid tooling..)

Also, note that the version of exference mentioned in the report had some bugs
that were fixed recently. No major changes were introduced, so the reader of the
report probably can use the latest version.

## Repositories

Currently available are two cabal packages, living in their own repositories:
- exference-exference-core: core functionality library
- exference-exference: both library with advanced interface and executable

Note that I have not published the irc bot package (yet).

## Compiling from source

Compiling the executable involves (after cloning the exference cabal package)
something along the lines of

~~~~
cabal sandbox init
cabal sandbox add-source exference-core
cabal install --deps
cabal configure -fbuild-executable
cabal build
# and, for example
cabal run -- "(Show b) => (a->b) -> [a] -> [String]"
~~~~

## Contributing

Note that i use a git subtree for integration of exference-core into exference.
(Maybe a submodule would be better, or just using a single repository -
i have no definite opinion yet). 

I am aware that few people know subtrees, and it does not really matter if
you do not, so do not be discouraged. Basically it means that changes
made to the exference-core subtree should be separated from other changes.
In contrast to submodules it is not necessary to `update` or `init`.

## Trivia

* The author did not learn about the term "entailment" until after implementing
  the respective part of the algorithm.

## IRC

`#exference`

## FAQ

- Why no hackage distribution?

    Best excuse is that i keep making minor modifications, and want to wait
    for a "proper" semi-finished version for a hackage publication.

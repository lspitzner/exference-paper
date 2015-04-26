# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\ b -> fmap (\ g -> show (b g))`

[Djinn](https://hackage.haskell.org/package/djinn) is a well known tool that
does something similar; the main difference is that *Exference* supports a
larger subset of the haskell type system - most prominently type classes. This
comes at a cost, however: *Exference* makes no promise regarding termination.
Where *Djinn* tells you "there are no solutions", exference will keep trying,
sometimes stopping with "i could not find any solutions".

# Links

- **Documentation: [exference.pdf](https://github.com/lspitzner/exference/raw/master/exference.pdf)** describes the implementation and properties;
- Source repositories
    - [exference-exference\-core](https://github.com/lspitzner/exference-exference-core): core functionality library
    - [exference-exference](https://github.com/lspitzner/exference-exference): both library with advanced interface and executable
- exferenceBot on freenode IRC #exference
    - play around without installing exference locally
    - reacts to `:exf` prefix, i.e. `:exf "Monad m => m (m a) -> m a"`
    - `/msg exferenceBot help`
    - uses the environment (i.e. known functions+typeclasses) at https://github.com/lspitzner/exference-exference/tree/master/environment

# Usage notes

There are certain types of queries where *Exference* will not be able to find
any / the right solution. Some common current limitations are:

- By default, searches **only for solutions where all input is used up**, e.g.
  `(a, b) -> a` will not find a solution (unless given `--allowunused` flag).
  Often this is the desired behaviour, consider queries such as
  `(a->b) -> [a] -> [b]` where a trivial solution would be `\_ _ -> []`.
  This also means that certain functions are not included in the environment,
  e.g. `length` or `mapM_`, as they "lose information";
- Type synonyms are not supported, e.g. `String -> [Char]` will not give
  solutions. Should be easy to implement, but I have not come around to it yet;
- Kinds are not checked, e.g. `Maybe -> Either`
  (which can be seen as both advantage and disadvantage, see report);
- The environment is composed by hand currently, and does only include parts
  of base plus a few other selected modules. Additions welcome!
- Pattern-matching on multiple-constructor data-types is not supported;
- See also the detailed feature description in the [exference.pdf](https://github.com/lspitzner/exference/raw/master/exference.pdf) report.

## Experimental features

- Pattern-matching on multi-constructor data types can be enabled via
  `-c --patternMatchMC`, but reduces performance significantly for any
  non-trivial queries. Core algorithm needs re-write to optimize stuff
  sufficiently I fear.
- I recently added support for RankNTypes, but this is largely untested.

## Other known (technical) issues

- **Memory consumption is large** (even more so when profiling);
- The executable is badly named (`test-main`);
  the tests should be put in a proper test-suite.
  (initially the executable was created for testing purposes
  , but now serves as command-line interface;
  this is why no parameters run tests.)
- The dependency bounds of the cabal packages should be cleaned up/checked.
  I postponed this as there is no automated way to do this. stupid tooling..)
- The package does not contain the environment via data-files, so the sdist
  package might be incomplete.

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
* *Exference* was used at least once to implement some typed hole in its own
  source code.

## IRC

`#exference`

## FAQ

- Why no hackage distribution?

    Best excuse is that i keep making minor modifications, and want to wait
    for a "proper" semi-finished version for a hackage publication.

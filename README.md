# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\ b -> fmap (\ g -> show (b g))`

This might seem similar to what [djinn](https://hackage.haskell.org/package/djinn)
does, but there are significant differences.

## Documentation

There is [an introductory paper/report](https://github.com/lspitzner/exference/exference.pdf)
which describes the exact features of the tool.

## Repositories

The sources of the pdf and the cabal packages are kept in their own repositories:
- exference-exference-doc: the paper markdown;
- exference-exference-core and
- exference-exference: cabal packages.

Note that I have not published the irc bot package (yet).

# coalaHs
Base for coala bears written in Haskell.

Needs Stack.

Rebuilding documentation with `stack haddock && cp -R .stack-work/dist/*/*/doc/html/coalaHs/* docs`

## How to program a bear

In principle, a bear has the following type: `coala :: String -> (ByteString -> Maybe a) -> (a -> [Result]) -> IO ()` or
`coalaIO :: String -> (ByteString -> Maybe a) -> (a -> IO [Result]) -> IO ()` where the first parameter is the name of your bear,
the second is a reader that we provide (for example `singleFile :: ByteString -> Maybe SingleFile`) and the third is
your program that interprets the result of the reader (if available) to a list of results, that are written to stdout for `Coala`
to use.

The most basic bear that does nothing would be `main = coala "doNothingBear" singleFile (const [])`.

The result type is constructed this way:

```
newtype Severity = Severity Int deriving ( Show, Eq )
newtype Line = Line Int deriving ( Show, Eq )
newtype Column = Column Int deriving ( Show, Eq )

data Affect = Affect  { start :: CodeRef
                      , end :: CodeRef
                      } deriving ( Eq, Show )

data CodeRef = CodeRef  { file :: Filename
                        , line :: Line
                        , column :: Maybe Column
                        }  deriving (Eq, Show)

data Result = Result  { message :: String
                      , affected :: [Affect]
                      , severity :: Severity
                      } deriving (Eq, Show)
```

To construct `Affect`s (and `CodeRef`s) more easily, we provide the following:

* `codeRef :: Filename -> (Line, Maybe Column) -> (Line, Maybe Column) -> Affect`
* `codeRefLine :: Filename -> Line -> Affect`
* `codeRefInLine :: Filename -> Line -> Maybe Column -> (Int -> Int) -> Affect`

You can look at an example bear at https://github.com/maweki/coalaCheckBear-hs

If you're using `coalaIO` as `main`, you should not write to `stdout`. Since we're
not in pureScript, we can not easily restrict what can be done in `IO [Result]`.

## Readers

```
type Settings = Map.Map String String
```

```
data SingleFile = SingleFile  { filename :: Filename -- ^ Source filename
                              , file :: [Text]     -- ^ Lines of source file
                              , settings :: Settings -- ^ Settings
                              } deriving ( Eq, Show )
```

## What needs to be done

First of all, coala has yet to have those bindings included. This is basically the prototype implementation for a Haskell-based bear-framework.

* We currently only provide a single reader, while Coala does have multiple possible output formats for bears
* The reader type (`ByteString -> Maybe a`) is very general. Maybe a typeclass restricting `a` would be nice
* More helpers
* Nicer names
* Nice structs (some lense-like accessors would be nicer than the exports we now have - but we also have very few dependencies)
* more documentation (when is it ever enough?)
* check whether we need a more permissible license for our users

-- | The graph: which module of `hylograph-selection` imports which (the 61 of
-- | its 71 modules that import or are imported by another, and the 87 imports
-- | between them), read from the PureScript
-- | compiler's output by `extract-imports.mjs` into `data/imports.json`.
-- |
-- | (The workspace Makefile was the first choice. The `baskerville-make`
-- | demo's parser reads it without trouble, and finds one dependency in it:
-- | `all: polyglot`. That is not a graph.)
-- |
-- | An import is a dependency, and imports cannot be circular, so this is a
-- | directed acyclic graph: there is a way of drawing it in which every
-- | module is above everything it imports.
module Downhill.Graph
  ( Graph
  , Module
  , load
  , decode
  ) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign.Object as Object

type Module =
  { name :: String -- in full: Hylograph.HATS.InterpreterTick
  , short :: String -- without the library's first word: HATS.InterpreterTick
  , family :: Int -- modules that share their next word share a family
  }

-- | `source` imports `target`, by their places in `modules`.
type Graph =
  { library :: String
  , modules :: Array Module
  , imports :: Array { source :: Int, target :: Int }
  }

load :: String -> Aff (Either String Graph)
load url = Affjax.get ResponseFormat.json url <#> case _ of
  Left problem -> Left (Affjax.printError problem)
  Right response -> note ("not an import graph: " <> url) (decode response.body)

decode :: Json -> Maybe Graph
decode json = do
  object <- Json.toObject json
  library <- Object.lookup "library" object >>= Json.toString
  everyName <- Object.lookup "modules" object >>= Json.toArray >>= traverse Json.toString
  pairs <- Object.lookup "imports" object >>= Json.toArray >>= traverse \entry -> do
    ends <- Json.toObject entry
    from <- Object.lookup "from" ends >>= Json.toString
    to <- Object.lookup "to" ends >>= Json.toString
    pure { from, to }
  let
    -- Only the modules that import or are imported by another of the
    -- library's: the rest are not in the graph.
    names = Array.filter (\name -> Array.any (\pair -> pair.from == name || pair.to == name) pairs) everyName
    words name = String.split (Pattern ".") name
    familyOf name = fromMaybe "" (Array.index (words name) 1)
    families = Array.nub (familyOf <$> names)
    modules = names <#> \name ->
      { name
      , short: String.joinWith "." (Array.drop 1 (words name))
      , family: fromMaybe 0 (Array.elemIndex (familyOf name) families)
      }
  imports <- traverse (\pair -> { source: _, target: _ } <$> Array.elemIndex pair.from names <*> Array.elemIndex pair.to names) pairs
  pure { library, modules, imports }

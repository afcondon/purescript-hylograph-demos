-- | The graph: who appears with whom in Les Misérables. 77 characters, 254
-- | links (Knuth's co-occurrence data, as d3's examples carry it).
module Territories.Story
  ( Story
  , Character(..)
  , Meeting
  , load
  , decode
  ) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign.Object as Object

newtype Character = Character String

derive newtype instance Eq Character
derive newtype instance Ord Character
derive newtype instance Show Character

-- | Two characters who share a scene.
type Meeting = { source :: Character, target :: Character }

type Story =
  { characters :: Array Character
  , meetings :: Array Meeting
  }

load :: String -> Aff (Either String Story)
load url = Affjax.get ResponseFormat.json url <#> case _ of
  Left problem -> Left (Affjax.printError problem)
  Right response -> note ("not a graph: " <> url) (decode response.body)

decode :: Json -> Maybe Story
decode json = do
  object <- Json.toObject json
  characters <- Object.lookup "nodes" object >>= Json.toArray >>= traverse \node ->
    Character <$> (Json.toObject node >>= Object.lookup "id" >>= Json.toString)
  meetings <- Object.lookup "links" object >>= Json.toArray >>= traverse \link -> do
    ends <- Json.toObject link
    source <- Object.lookup "source" ends >>= Json.toString
    target <- Object.lookup "target" ends >>= Json.toString
    pure { source: Character source, target: Character target }
  pure { characters, meetings }

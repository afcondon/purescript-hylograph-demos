-- | The graph: who appears with whom in *Cien años de soledad*, as
-- | `~/work/afc-work/macondo` derives it. Shaped like Knuth's Les Misérables
-- | data, with more fields on each person: `generation` (a Buendía's place in
-- | the tree, null for anyone else), `era` (the generation a person shares
-- | most passages with; a Buendía's own), `sex`, `world` (where they are
-- | present most) and `appearances`.
module Macondo.Story
  ( Story
  , Person
  , Sex(..)
  , World(..)
  , allWorlds
  , worldName
  , Meeting
  , Couple
  , load
  , decode
  ) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Int (round)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object

type Person =
  { id :: String
  , name :: String
  , family :: Boolean -- a Buendía, or the spouse or other parent of one's child
  , era :: Int
  , sex :: Sex
  , world :: Maybe World -- the world they are present in most
  , appearances :: Int
  , parents :: Array String -- ids; a Buendía's, where the tree gives them
  , first :: Int -- the passage, counted through the novel, where they are first present
  , firstChapter :: Int
  }

data Sex = Man | Woman

derive instance Eq Sex

-- | Where in the novel a passage is set, in five: the Buendía house (with
-- | Melquíades's room); the town (streets, other houses, Catarino's, Petra
-- | Cotes's, the church); power (the barracks and the war); the banana
-- | company (the plantations and the station); and away (before the founding,
-- | Fernanda's city, Europe).
data World = House | Town | Power | Company | Away

derive instance Eq World

allWorlds :: Array World
allWorlds = [ Away, House, Town, Power, Company ]

worldName :: World -> String
worldName = case _ of
  House -> "The house"
  Town -> "The town"
  Power -> "Power and war"
  Company -> "The company"
  Away -> "Away"

-- | Two people present in the same passages, `passages` times.
type Meeting = { source :: String, target :: String, passages :: Int }

-- | Two people the family tree joins: the parents of a child, or a couple
-- | the novel leaves childless.
type Couple = { a :: String, b :: String }

type Story =
  { people :: Array Person
  , meetings :: Array Meeting
  , couples :: Array Couple
  }

load :: String -> Aff (Either String Story)
load url = Affjax.get ResponseFormat.json url <#> case _ of
  Left problem -> Left (Affjax.printError problem)
  Right response -> note ("not the Macondo graph: " <> url) (decode response.body)

decode :: Json -> Maybe Story
decode json = do
  object <- Json.toObject json
  people <- Object.lookup "nodes" object >>= Json.toArray >>= traverse \node -> do
    fields <- Json.toObject node
    id <- string "id" fields
    -- The page is in Spanish: the name as the novel gives it.
    name <- string "name_es" fields
    era <- int "era" fields
    appearances <- int "appearances" fields
    sex <- string "sex" fields >>= case _ of
      "m" -> Just Man
      "f" -> Just Woman
      _ -> Nothing
    let
      world = Object.lookup "world" fields >>= Json.toString >>= case _ of
        "house" -> Just House
        "town" -> Just Town
        "power" -> Just Power
        "company" -> Just Company
        "away" -> Just Away
        _ -> Nothing
    let family = isJust (Object.lookup "generation" fields >>= Json.toNumber)
    parents <- Object.lookup "parents" fields >>= Json.toArray >>= traverse Json.toString
    first <- int "first" fields
    firstChapter <- int "first_chapter" fields
    pure { id, name, family, era, sex, world, appearances, parents, first, firstChapter }
  meetings <- Object.lookup "links" object >>= Json.toArray >>= traverse \link -> do
    fields <- Json.toObject link
    source <- string "source" fields
    target <- string "target" fields
    passages <- int "value" fields
    pure { source, target, passages }
  couples <- Object.lookup "couples" object >>= Json.toArray >>= traverse \couple -> do
    fields <- Json.toObject couple
    a <- string "a" fields
    b <- string "b" fields
    pure { a, b }
  pure { people, meetings, couples }
  where
  string :: String -> Object Json -> Maybe String
  string key fields = Object.lookup key fields >>= Json.toString

  int :: String -> Object Json -> Maybe Int
  int key fields = round <$> (Object.lookup key fields >>= Json.toNumber)

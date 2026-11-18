-- | The data: the wealth and health of nations. Income per person, life
-- | expectancy and population for 180 countries in four years, from the
-- | Gapminder series as d3's "The Wealth & Health of Nations" example carries
-- | them (the workspace's copy is in the polyglot site's assets; the four
-- | years were taken from it into `public/data/nations.json`, interpolating
-- | where a series has no reading for the year itself).
module Labels.Nations
  ( Nation
  , Reading
  , Continent(..)
  , Era(..)
  , allEras
  , eraLabel
  , readingIn
  , load
  , decode
  ) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign.Object as Object

-- | The regions the data sorts countries into.
data Continent
  = SubSaharanAfrica
  | EuropeAndCentralAsia
  | America
  | EastAsiaAndPacific
  | MiddleEastAndNorthAfrica
  | SouthAsia

derive instance Eq Continent

-- | The years on offer.
data Era
  = In1900
  | In1950
  | In1980
  | In2008

derive instance Eq Era

allEras :: Array Era
allEras = [ In1900, In1950, In1980, In2008 ]

eraLabel :: Era -> String
eraLabel = case _ of
  In1900 -> "1900"
  In1950 -> "1950"
  In1980 -> "1980"
  In2008 -> "2008"

type Reading =
  { income :: Number -- per person, in dollars
  , lifeExpectancy :: Number -- in years
  , population :: Number
  }

type Nation =
  { name :: String
  , continent :: Continent
  , in1900 :: Maybe Reading
  , in1950 :: Maybe Reading
  , in1980 :: Maybe Reading
  , in2008 :: Maybe Reading
  }

-- | A nation's reading for a year, if the data has one.
readingIn :: Era -> Nation -> Maybe Reading
readingIn = case _ of
  In1900 -> _.in1900
  In1950 -> _.in1950
  In1980 -> _.in1980
  In2008 -> _.in2008

load :: String -> Aff (Either String (Array Nation))
load url = Affjax.get ResponseFormat.json url <#> case _ of
  Left problem -> Left (Affjax.printError problem)
  Right response -> note ("not the nations: " <> url) (decode response.body)

decode :: Json -> Maybe (Array Nation)
decode json = Json.toObject json >>= Object.lookup "nations" >>= Json.toArray >>= traverse \entry -> do
  nation <- Json.toObject entry
  name <- Object.lookup "name" nation >>= Json.toString
  continent <- Object.lookup "region" nation >>= Json.toString >>= continentNamed
  readings <- Object.lookup "readings" nation >>= Json.toObject
  let
    reading era = do
      found <- Object.lookup (eraLabel era) readings >>= Json.toObject
      income <- Object.lookup "income" found >>= Json.toNumber
      lifeExpectancy <- Object.lookup "lifeExpectancy" found >>= Json.toNumber
      population <- Object.lookup "population" found >>= Json.toNumber
      pure { income, lifeExpectancy, population }
  pure { name, continent, in1900: reading In1900, in1950: reading In1950, in1980: reading In1980, in2008: reading In2008 }

continentNamed :: String -> Maybe Continent
continentNamed = case _ of
  "Sub-Saharan Africa" -> Just SubSaharanAfrica
  "Europe & Central Asia" -> Just EuropeAndCentralAsia
  "America" -> Just America
  "East Asia & Pacific" -> Just EastAsiaAndPacific
  "Middle East & North Africa" -> Just MiddleEastAndNorthAfrica
  "South Asia" -> Just SouthAsia
  _ -> Nothing

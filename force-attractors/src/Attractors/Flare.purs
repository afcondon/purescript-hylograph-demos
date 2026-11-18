-- | The hierarchy: flare-2, 252 nodes, read from JSON into a rose tree that
-- | already knows what every layout and the picture will ask of a node.
module Attractors.Flare
  ( Flare(..)
  , NodePath(..)
  , Member
  , load
  , decode
  , members
  , isLeaf
  ) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Aff (Aff)
import Foreign.Object as Object

-- | A node's name with its ancestors' ("flare.animate.Tween"): its identity
-- | in every layout.
newtype NodePath = NodePath String

derive newtype instance Eq NodePath
derive newtype instance Ord NodePath
derive newtype instance Show NodePath

newtype Flare = Flare
  { path :: NodePath
  , name :: String
  , value :: Number -- a leaf's own size; an internal node's is the sum of its leaves
  , depth :: Int
  , branch :: Maybe Int -- which child of the root this node descends from
  , children :: Array Flare
  }

isLeaf :: Flare -> Boolean
isLeaf (Flare node) = Array.null node.children

-- | A node without its children, and with its parent's path instead.
type Member =
  { path :: NodePath
  , parent :: Maybe NodePath
  , value :: Number
  , depth :: Int
  , branch :: Maybe Int
  }

-- | Every node, parents before children.
members :: Flare -> Array Member
members = go Nothing
  where
  go parent (Flare node) =
    [ { path: node.path, parent, value: node.value, depth: node.depth, branch: node.branch } ]
      <> Array.concatMap (go (Just node.path)) node.children

load :: String -> Aff (Either String Flare)
load url = Affjax.get ResponseFormat.json url <#> case _ of
  Left problem -> Left (Affjax.printError problem)
  Right response -> note ("not a hierarchy: " <> url) (decode response.body)

decode :: Json -> Maybe Flare
decode = go "" 0 Nothing
  where
  go :: String -> Int -> Maybe Int -> Json -> Maybe Flare
  go prefix depth branch json = do
    object <- Json.toObject json
    name <- Object.lookup "name" object >>= Json.toString
    let
      path = prefix <> name
      ownValue = fromMaybe 0.0 (Object.lookup "value" object >>= Json.toNumber)
      below index = go (path <> ".") (depth + 1) (if depth == 0 then Just index else branch)
    children <- case Object.lookup "children" object >>= Json.toArray of
      Just array -> traverseWithIndex below array
      Nothing -> Just []
    pure $ Flare
      { path: NodePath path
      , name
      , value: if Array.null children then ownValue else sum (children <#> \(Flare child) -> child.value)
      , depth
      , branch
      , children
      }

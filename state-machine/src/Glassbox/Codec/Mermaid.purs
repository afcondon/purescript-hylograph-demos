-- | Glassbox.Codec.Mermaid
-- |
-- | Emit a described machine as Mermaid `stateDiagram-v2`.
-- |
-- | Output only, and deliberately the first codec: Mermaid renders free in
-- | GitHub and in Artifacts, which makes it the cheapest available second
-- | opinion on whether the derived description is faithful. If the SVG and the
-- | Mermaid disagree, one of the two readings is wrong — and finding that out
-- | costs nothing here.
module Glassbox.Codec.Mermaid
  ( toMermaid
  ) where

import Prelude

import Data.Array (filter, find)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import DataViz.Layout.StateMachine (StateMachine)
import Glassbox.Describe (EdgeExtra, EdgeKind(..))

toMermaid :: StateMachine Unit EdgeExtra -> String
toMermaid machine =
  joinWith "\n" $
    [ "stateDiagram-v2" ]
      <> map stateLine machine.states
      <> initialLine
      <> map edgeLine (filter drawn machine.transitions)
  where
  -- Refusals are self-loops that would triple the edge count without telling a
  -- reader anything the SVG's refusal styling does not already say.
  drawn edge = edge.extra.kind /= OnRefusal

  stateLine state = "    " <> state.id <> " : " <> clean state.label

  initialLine = case find _.isInitial machine.states of
    Just initial -> [ "    [*] --> " <> initial.id ]
    Nothing -> []

  edgeLine edge =
    "    " <> edge.from <> " --> " <> edge.to <> " : " <> marker edge.extra.kind <> clean edge.label

  marker = case _ of
    OnDeadline -> "after "
    _ -> ""

  -- A colon separates a Mermaid transition from its label, so it cannot appear
  -- inside one.
  clean = replaceAll (Pattern ":") (Replacement " ")

-- | Glassbox.Export.StateDiagram
-- |
-- | Emit a described machine as state-diagram text.
-- |
-- | **Nothing is imported to do this.** The dialect is Mermaid's
-- | `stateDiagram-v2`, because GitHub and a hundred other things render it for
-- | free, but Mermaid is a *paste target* and not a dependency: what follows is
-- | string concatenation over our own value, and the bundle pulls in no
-- | external package to produce it. The drawing on screen is ours too — derived
-- | from the same value, laid out by `DataViz.Layout.StateMachine`, and put on
-- | the page by HATS. This module is a convenience for getting a machine into
-- | somebody else's document, not a way of getting out of drawing it.
-- |
-- | It lives under `Export` rather than `Codec` because it only goes one way. A
-- | codec round-trips; this cannot be read back, and filing it beside
-- | `Glassbox.Codec.JSON` implied a symmetry it does not have.
-- |
-- | Its cheap value is as a second opinion: if the picture and this text
-- | disagree, one of the two readings is wrong, and finding that out costs a
-- | glance.
module Glassbox.Export.StateDiagram
  ( toStateDiagram
  ) where

import Prelude

import Data.Array (filter, find)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import DataViz.Layout.StateMachine (StateMachine)
import Glassbox.Describe (EdgeExtra, EdgeKind(..))

toStateDiagram :: StateMachine Unit EdgeExtra -> String
toStateDiagram machine =
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

  -- A colon separates a transition from its label, so it cannot appear
  -- inside one.
  clean = replaceAll (Pattern ":") (Replacement " ")

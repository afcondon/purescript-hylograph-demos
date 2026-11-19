-- | A dependency graph that flows downhill, and a simulation you can scrub.
-- |
-- | The graph arranges itself under forces, one of which pushes every import
-- | to point down. The run is recorded: a snapshot every few ticks. The
-- | slider goes back to any of them; from there a module can be pinned and
-- | the simulation played on into a different future, with the first run
-- | kept behind it as a ghost. Nothing is random, so going back without
-- | changing anything and playing again lives the same run again, exactly.
module Downhill.Main (main) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Downhill.Chart (controls, nodesChart, readout)
import Downhill.Graph (Graph)
import Downhill.Graph as Graph
import Downhill.Physics (Body, BodyFields, bodies, downhill, physics)
import Downhill.Timeline (Moment, Timeline)
import Downhill.Timeline as Timeline
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Simulation.Stepped (Stepped)
import Hylograph.Simulation.Stepped as Stepped
import Hylograph.Transition.RAF (requestAnimationFrame)

type State =
  { timeline :: Timeline -- the run being shown
  , firstRun :: Maybe Timeline -- kept whole once it has ended, for the ghost
  , bodies :: Array Body -- as the simulation has them now
  , tick :: Int
  , playing :: Boolean
  , branched :: Boolean
  }

main :: Effect Unit
main = launchAff_ do
  loaded <- Graph.load "./data/imports.json"
  case loaded of
    Left problem -> liftEffect (Console.error problem)
    Right graph -> liftEffect (run graph)

run :: Graph -> Effect Unit
run graph = do
  let starting = bodies graph
  machine <- Stepped.create Stepped.Default physics starting graph.imports
  first <- momentOf machine 0 starting
  state <- Ref.new
    { timeline: Timeline.begin first
    , firstRun: Nothing
    , bodies: starting
    , tick: 0
    , playing: true
    , branched: false
    }

  let
    draw = do
      current <- Ref.read state
      finished <- machine.cooled
      alpha <- machine.alpha
      let
        pinned = pinnedIn current.bodies
        reading =
          { tick: current.tick
          , moments: Array.length current.timeline.moments
          , shown: current.timeline.shown
          , playing: current.playing
          , finished
          , branched: current.branched
          , firstRunKept: isJust current.firstRun
          , pinned: Array.length pinned
          , alpha
          , downhill: downhill graph (current.bodies <#> \body -> { x: body.x, y: body.y })
          , imports: Array.length graph.imports
          }
      void $ rerender "#stage" $ nodesChart graph pressModule
        { bodies: current.bodies
        , pinned
        , ghost: current.firstRun >>= Timeline.momentAt current.tick <#> _.places
        }
      void $ rerender "#controls" (controls { play, pause, scrub } reading)
      void $ rerender "#readout" (readout reading)

    -- Go on from the moment shown. What was recorded after it is forgotten
    -- and recorded again as it is lived; unless something was changed, it is
    -- the same.
    play = do
      current <- Ref.read state
      finished <- machine.cooled
      unless finished do
        Ref.write (current { timeline = Timeline.cutAfterNow current.timeline, playing = true }) state
      draw

    -- Stop, keeping this moment.
    pause = do
      current <- Ref.read state
      when current.playing do
        timeline <- noted current
        Ref.write (current { timeline = timeline, playing = false }) state
      draw

    -- Go to a recorded moment: the simulation is put back as it was then.
    scrub place = do
      current <- Ref.read state
      let timeline = Timeline.goTo place current.timeline
      case Timeline.now timeline of
        Just moment -> do
          restored <- machine.restore moment.snapshot
          Ref.write (current { timeline = timeline, bodies = restored, tick = moment.tick, playing = false }) state
        Nothing -> pure unit
      draw

    -- Pin a module where it is, or let a pinned one go. Only once the first
    -- run has been kept, and not while running: the moment shown is changed,
    -- and what followed it no longer follows.
    pressModule place = do
      current <- Ref.read state
      when (isJust current.firstRun && not current.playing) do
        case Array.index current.bodies place of
          Just body -> do
            machine.pin body.id (if Array.elem place (pinnedIn current.bodies) then Nothing else Just { x: body.x, y: body.y })
            -- Read the simulation back through a snapshot of itself: the
            -- nodes with the pin, as the next tick will find them.
            changed <- machine.snapshot
            pinnedNow <- machine.restore changed
            let moment = { tick: current.tick, snapshot: changed, places: placesOf pinnedNow, pinned: pinnedIn pinnedNow }
            Ref.write (current { timeline = Timeline.replaceNow moment current.timeline, bodies = pinnedNow, branched = true }) state
          Nothing -> pure unit
        draw

    -- The moment the simulation is at, added to the timeline unless it is
    -- the one already shown.
    noted current = case Timeline.now current.timeline of
      Just shown | shown.tick == current.tick -> pure current.timeline
      _ -> do
        moment <- momentOf machine current.tick current.bodies
        pure (Timeline.record moment current.timeline)

    frame _ = do
      current <- Ref.read state
      when current.playing do
        next <- machine.step
        finished <- machine.cooled
        let
          tick = current.tick + 1
          stepped = current { bodies = next, tick = tick }
        timeline <-
          if finished || tick `mod` Timeline.every == 0 then noted stepped
          else pure current.timeline
        Ref.write
          ( stepped
              { timeline = timeline
              , playing = not finished
              , firstRun = case current.firstRun of
                  Nothing | finished -> Just timeline
                  kept -> kept
              }
          )
          state
        draw
      void $ requestAnimationFrame frame

  draw
  void $ requestAnimationFrame frame

momentOf :: Stepped BodyFields -> Int -> Array Body -> Effect Moment
momentOf machine tick present = do
  snapshot <- machine.snapshot
  pure { tick, snapshot, places: placesOf present, pinned: pinnedIn present }

placesOf :: Array Body -> Array { x :: Number, y :: Number }
placesOf = map \body -> { x: body.x, y: body.y }

-- | The places in the node array of the nodes that are pinned.
pinnedIn :: Array Body -> Array Int
pinnedIn = Array.mapMaybe \body -> case toMaybe body.fx of
  Just _ -> Just body.id
  Nothing -> Nothing

-- | Communities as territory.
-- |
-- | One graph, drawn twice by the same simulation: as an ordinary
-- | force-directed graph (open country), and with its decomposition into
-- | blocks enforced as geography (territories). The button changes the
-- | forces on the running simulation and nothing else; the characters walk
-- | from the one arrangement to the other.
module Territories.Main (main) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect, untilE)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Hylograph.ForceEngine.Setup (Setup)
import Hylograph.ForceEngine.Setup.WASM (WASMSim)
import Hylograph.ForceEngine.Setup.WASM as Simulation
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Transition.RAF (requestAnimationFrame)
import Territories.Chart (controls, nodesChart, readout)
import Territories.Geography (Geography)
import Territories.Geography as Geography
import Territories.Physics (Mode(..), atRest, cooling, physics)
import Territories.Physics as Physics
import Territories.Story (Story)
import Territories.Story as Story

type State =
  { mode :: Mode
  , ticks :: Int
  , settled :: Boolean
  }

main :: Effect Unit
main = launchAff_ do
  loaded <- Story.load "./data/miserables.json"
  case loaded of
    Left problem -> liftEffect (Console.error problem)
    Right story -> do
      Simulation.initWasm "./pkg/force_kernel.js"
      liftEffect (run story)

run :: Story -> Effect Unit
run story = do
  let geography = Geography.geography story
  simulation <- rehearse geography story
  state <- Ref.new { mode: Territories, ticks: 0, settled: true }

  let
    draw bodies = do
      current <- Ref.read state
      maxDisplacement <- Simulation.maxDisplacement simulation
      maxAcceleration <- Simulation.maxAcceleration simulation
      void $ rerender "#stage" (nodesChart geography story current.mode bodies)
      void $ rerender "#readout" $ readout geography
        { mode: current.mode, ticks: current.ticks, maxDisplacement, maxAcceleration, settled: current.settled }

    -- Change the forces. The simulation keeps running: positions and
    -- velocities are what they were.
    choose mode = do
      current <- Ref.read state
      when (mode /= current.mode) do
        Simulation.applySetup (physics geography mode) simulation
        Ref.write { mode, ticks: 0, settled: false } state
        void $ rerender "#controls" (controls choose mode)

    frame _ = do
      current <- Ref.read state
      unless current.settled do
        bodies <- Simulation.step simulation
        settled <- Simulation.settled atRest simulation
        Ref.write (current { ticks = current.ticks + 1, settled = settled }) state
        draw bodies
      void $ requestAnimationFrame frame

  void $ rerender "#controls" (controls choose Territories)
  draw =<< Simulation.getNodes simulation
  void $ requestAnimationFrame frame

-- | Before anything is drawn: find where the characters come to rest in each
-- | mode, give each character those two places to remember (see
-- | `Physics.physics`), and walk there and back until the walk repeats.
-- | Returns the simulation at rest in its territories.
rehearse :: Geography -> Story -> Effect (WASMSim Physics.BodyFields)
rehearse geography story = do
  finding <- Simulation.create (Physics.bodies geography story) (Physics.links story)
  inTerritories <- rest (physics geography Territories) finding
  Simulation.applySetup (cooling (physics geography OpenCountry)) finding
  _ <- Simulation.tickN coolingTicks finding
  inOpenCountry <- Simulation.getNodes finding
  Simulation.free finding

  let
    remembering = Array.zipWith
      ( \here there -> here
          { vx = 0.0
          , vy = 0.0
          , remembered = Just { territories: { x: here.x, y: here.y }, openCountry: { x: there.x, y: there.y } }
          }
      )
      inTerritories
      inOpenCountry
  walking <- Simulation.create remembering (Physics.links story)
  for_ (Array.replicate roundTrips unit) \_ -> do
    void $ rest (physics geography OpenCountry) walking
    void $ rest (physics geography Territories) walking
  pure walking

-- | Apply a setup and tick until the simulation is at rest (or has plainly
-- | not come to rest: the limit is far beyond any walk here).
rest :: forall fields. Setup (Simulation.SimulationNode fields) -> WASMSim fields -> Effect (Array (Simulation.SimulationNode fields))
rest setup simulation = do
  Simulation.applySetup setup simulation
  ticks <- Ref.new 0
  untilE do
    void $ Simulation.tick simulation
    made <- Ref.modify (_ + 1) ticks
    settled <- Simulation.settled atRest simulation
    pure (settled || made >= tickLimit)
  Simulation.getNodes simulation

coolingTicks :: Int
coolingTicks = 400

roundTrips :: Int
roundTrips = 3

tickLimit :: Int
tickLimit = 6000

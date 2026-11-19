-- | Labels that never collide.
-- |
-- | A scatterplot in which every point is named, and the names are bodies in
-- | a force simulation: rectangles that are drawn to their points, cannot
-- | overlap each other or any point, and cannot leave the plot. Change the
-- | year and the points move; the labels rearrange while the simulation
-- | runs.
module Labels.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Transition.RAF (performanceNow, requestAnimationFrame)
import Labels.Chart (controls, nodesChart, readout, summarise)
import Hylograph.Simulation.Stepped (Stepped, initWasm)
import Hylograph.Simulation.Stepped as Stepped
import Labels.Engine (Kernel(..), engineOf)
import Labels.Nations (Era(..), Nation)
import Labels.Nations as Nations
import Labels.Physics (Body, BodyFields, Placing(..), Stage(..), atRest, bodies, firstStage, forcesOf, nextStage)
import Labels.Plot (dotsIn)

type State =
  { era :: Era
  , placing :: Placing
  , stage :: Stage
  , kernel :: Kernel
  , engine :: Stepped BodyFields
  , bodies :: Array Body
  , ticks :: Int -- since the last change
  , spent :: Number -- milliseconds in the kernel since the last change
  , settled :: Boolean
  , millisecondsPerTick :: { pureScript :: Maybe Number, rust :: Maybe Number }
  }

main :: Effect Unit
main = launchAff_ do
  loaded <- Nations.load "./data/nations.json"
  case loaded of
    Left problem -> liftEffect (Console.error problem)
    Right nations -> do
      initWasm "./pkg/force_kernel.js"
      liftEffect (run nations)

-- | How many nations are shown: every one that has a reading for the year
-- | (145 in 1900, 174 to 178 since).
shown :: Int
shown = 180

run :: Array Nation -> Effect Unit
run nations = do
  let
    firstBodies = bodies [] (dotsIn shown In2008 nations)
  firstEngine <- Stepped.create (engineOf PureScript) (forcesOf Free (firstStage Free)) firstBodies []
  state <- Ref.new
    { era: In2008
    , placing: Free
    , stage: firstStage Free
    , kernel: PureScript
    , engine: firstEngine
    , bodies: firstBodies
    , ticks: 0
    , spent: 0.0
    , settled: false
    , millisecondsPerTick: { pureScript: Nothing, rust: Nothing }
    }

  let
    draw = do
      current <- Ref.read state
      void $ rerender "#stage" (nodesChart current.bodies)
      void $ rerender "#readout" $ readout
        { summary: summarise current.bodies
        , kernel: current.kernel
        , ticks: current.ticks
        , settled: current.settled
        , millisecondsPerTick: current.millisecondsPerTick
        }

    drawControls = do
      current <- Ref.read state
      void $ rerender "#controls" $
        controls { era: chooseEra, placing: choosePlacing, kernel: chooseKernel }
          { era: current.era, placing: current.placing, kernel: current.kernel }

    -- Another year: other dots, in other places. The labels that are still
    -- wanted set off from where they are.
    chooseEra era = do
      current <- Ref.read state
      let next = bodies current.bodies (dotsIn shown era nations)
      restart (current { era = era }) current.kernel next

    -- Other forces on the same bodies.
    choosePlacing placing = do
      current <- Ref.read state
      let stage = firstStage placing
      current.engine.applySetup (forcesOf placing stage)
      Ref.write (current { placing = placing, stage = stage, ticks = 0, spent = 0.0, settled = false }) state
      drawControls

    -- The other kernel, from where this one has got to.
    chooseKernel kernel = do
      current <- Ref.read state
      restart current kernel current.bodies

    -- A new simulation of these bodies, beginning the stages again.
    restart current kernel next = do
      let stage = firstStage current.placing
      current.engine.free
      engine <- Stepped.create (engineOf kernel) (forcesOf current.placing stage) next []
      Ref.write (current { kernel = kernel, engine = engine, bodies = next, stage = stage, ticks = 0, spent = 0.0, settled = false }) state
      drawControls

    frame _ = do
      current <- Ref.read state
      unless current.settled do
        began <- performanceNow
        next <- current.engine.step
        ended <- performanceNow
        still <- current.engine.settled atRest
        let
          ticks = current.ticks + 1
          spent = current.spent + (ended - began)
          mean = Just (spent / toNumber ticks)
          onward = nextStage current.stage
          -- At rest when the last stage has stopped moving and, if the
          -- labels are free, nothing overlaps: while the spring and the
          -- charges are still warm the collision gives a little, and a
          -- moment's stillness then is not yet rest.
          settled = onward.stage == Packing && not onward.changed && still
            && (current.placing == AtTheirDots || (summarise next).overlaps == 0)
        when onward.changed (current.engine.applySetup (forcesOf current.placing onward.stage))
        Ref.write
          ( current
              { bodies = next
              , stage = onward.stage
              , ticks = ticks
              , spent = spent
              , settled = settled
              , millisecondsPerTick = case current.kernel of
                  PureScript -> current.millisecondsPerTick { pureScript = mean }
                  Rust -> current.millisecondsPerTick { rust = mean }
              }
          )
          state
        draw
      void $ requestAnimationFrame frame

  drawControls
  draw
  void $ requestAnimationFrame frame

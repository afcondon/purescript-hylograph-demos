-- | Layouts as attractors.
-- |
-- | A layout is a pure function from the hierarchy to a point for every node.
-- | The simulation holds one spring per node, from where the node is to where
-- | the current layout puts it. Choosing another layout moves the far end of
-- | every spring and nothing else: positions and velocities stay as they
-- | were, so the nodes swing from one arrangement into the next.
module Attractors.Main (main) where

import Prelude

import Attractors.Chart (Body, controls, nodesChart, radiusOf, readout)
import Attractors.Flare (Flare, members)
import Attractors.Flare as Flare
import Attractors.Layout (Arrangement, Layout(..), Point, arrange)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (null)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Hylograph.ForceEngine.Setup (Setup, collide, dynamic, setupWithParams, static, towards, withRadius, withStrength)
import Hylograph.ForceEngine.Setup.WASM as Simulation
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Transition.RAF (requestAnimationFrame)

-- ============================================================================
-- The physics
-- ============================================================================

-- | A spring to each node's target and a collision between the discs' cores.
-- |
-- | Each tick the kernel does, for every node,
-- |
-- |     velocity = velocityRetention * (velocity + springStrength * alpha * (target - position))
-- |     position = position + velocity
-- |
-- | Alpha is held at 1, so the spring is as stiff at the end of a flight as at
-- | its start. With these two numbers the motion is underdamped: a node
-- | overshoots its target by about a tenth of the distance it came, swings
-- | back, and is at rest in about two and a half seconds at sixty ticks a
-- | second.
physics :: Arrangement -> Setup Body
physics arrangement =
  setupWithParams "attractors"
    [ towards "towards" (targetIn arrangement) # withStrength (static springStrength)
    , collide "collide" # withRadius (dynamic core) # withStrength (static collisionStrength)
    ]
    { alpha: 1.0, alphaTarget: 1.0, alphaDecay: 0.0, alphaMin: 0.001, velocityRetention }

springStrength :: Number
springStrength = 0.012

-- | The share of its velocity a node keeps from one tick to the next.
velocityRetention :: Number
velocityRetention = 0.88

collisionStrength :: Number
collisionStrength = 0.3

-- | The part of a disc that collides: the inner half. Every layout's targets
-- | are further apart than two cores, so the collision deflects nodes that
-- | meet in flight and holds none of them off its target.
core :: Body -> Number
core body = body.radius / 2.0

-- | When the picture counts as at rest: no node moved further than `step` in
-- | the last tick, and no node's step changed by more than `acceleration`.
-- | The second is what makes it a test of rest. The nodes swing in step, and
-- | at the far end of each swing all of them are still at once; there the
-- | acceleration is at its largest. It is the spring's stiffness times the
-- | distance from the target that is too small to see (a twentieth of a
-- | pixel).
atRest :: { step :: Number, acceleration :: Number }
atRest = { step: 0.01, acceleration: springStrength * 0.05 }

targetIn :: Arrangement -> Body -> Maybe Point
targetIn arrangement body = Map.lookup body.path arrangement.targets

-- ============================================================================
-- The loop
-- ============================================================================

type State =
  { layout :: Layout
  , arrangement :: Arrangement
  , ticks :: Int -- since the layout was chosen
  , settled :: Boolean
  }

main :: Effect Unit
main = launchAff_ do
  loaded <- Flare.load "./data/flare-2.json"
  case loaded of
    Left problem -> liftEffect (Console.error problem)
    Right flare -> do
      Simulation.initWasm "./pkg/force_kernel.js"
      liftEffect (run flare)

run :: Flare -> Effect Unit
run flare = do
  let
    first = TidyTree
    start = arrange first flare
  simulation <- Simulation.create (bodiesAt start flare) []
  Simulation.applySetup (physics start) simulation
  state <- Ref.new { layout: first, arrangement: start, ticks: 0, settled: true }

  let
    draw bodies = do
      current <- Ref.read state
      kineticEnergy <- Simulation.kineticEnergy simulation
      maxDisplacement <- Simulation.maxDisplacement simulation
      maxAcceleration <- Simulation.maxAcceleration simulation
      void $ rerender "#stage" (nodesChart current.arrangement bodies)
      void $ rerender "#readout" $ readout
        { layout: current.layout
        , ticks: current.ticks
        , kineticEnergy
        , maxDisplacement
        , maxAcceleration
        , furthestFromTarget: furthestFrom current.arrangement bodies
        , settled: current.settled
        }

    -- Move the far end of every spring. The simulation is not rebuilt and
    -- its nodes are not touched.
    choose layout = do
      let arrangement = arrange layout flare
      void $ Simulation.retarget "towards" (targetIn arrangement) simulation
      Simulation.reheat simulation
      Ref.write { layout, arrangement, ticks: 0, settled: false } state
      void $ rerender "#controls" (controls choose layout)

    -- One tick of the simulation for each frame of the display.
    frame _ = do
      current <- Ref.read state
      unless current.settled do
        bodies <- Simulation.step simulation
        settled <- Simulation.settled atRest simulation
        Ref.write (current { ticks = current.ticks + 1, settled = settled }) state
        draw bodies
      void $ requestAnimationFrame frame

  void $ rerender "#controls" (controls choose first)
  draw =<< Simulation.getNodes simulation
  void $ requestAnimationFrame frame

-- | The hierarchy's nodes as bodies at rest on an arrangement's targets.
bodiesAt :: Arrangement -> Flare -> Array Body
bodiesAt arrangement flare = members flare # Array.mapWithIndex \index member ->
  let
    point = fromMaybe { x: 0.0, y: 0.0 } (Map.lookup member.path arrangement.targets)
  in
    { id: index
    , x: point.x
    , y: point.y
    , vx: 0.0
    , vy: 0.0
    , fx: null
    , fy: null
    , path: member.path
    , parent: member.parent
    , radius: radiusOf member.value
    , branch: member.branch
    }

furthestFrom :: Arrangement -> Array Body -> Number
furthestFrom arrangement bodies = fromMaybe 0.0 $ maximum $ bodies # Array.mapMaybe \body ->
  targetIn arrangement body <#> \target ->
    sqrt ((body.x - target.x) * (body.x - target.x) + (body.y - target.y) * (body.y - target.y))

-- | The simulation, with what a timeline needs of it: step, snapshot,
-- | restore, pin.
-- |
-- | This is the PureScript kernel (the default engine). Its nodes are f64
-- | and its snapshots are immutable PureScript values, so a past moment can
-- | be kept for as long as wanted and gone back to exactly.
-- |
-- | WORKAROUNDS (hylograph-simulation): the PureScript engine
-- | (`Hylograph.ForceEngine.Simulation`) has no `snapshot`, `restore`,
-- | `step`, or way to pin one node; the Rust engine's wrapper has the first
-- | three. They are done here with `hylograph-forces-pure`'s own
-- | (`Snapshot.take`, `Snapshot.restore`, `Node.writePin`) on the
-- | simulation's fields (`random`, `alpha`, `nodes`), which are public only
-- | because `Simulation` is a record type. The engine also writes into the
-- | node records it is given and returns the same records after every tick,
-- | so nodes are copied on the way in and out.
module Downhill.Machine
  ( Machine
  , start
  , step
  , finished
  , alpha
  , snapshot
  , restore
  , pin
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn3)
import Hylograph.ForceEngine.Setup (Setup)
import Hylograph.ForceEngine.Setup as Setup
import Hylograph.ForceEngine.Simulation (Simulation, SimulationNode)
import Hylograph.ForceEngine.Simulation as Pure
import Hylograph.Forces.Pure.Node (writePin)
import Hylograph.Forces.Pure.Snapshot (Snapshot)
import Hylograph.Forces.Pure.Snapshot as Snapshot

newtype Machine fields = Machine { simulation :: Simulation fields (), alphaMin :: Number }

start
  :: forall fields
   . Setup (SimulationNode fields)
  -> Array (SimulationNode fields)
  -> Array { source :: Int, target :: Int }
  -> Effect (Machine fields)
start setup nodes links = do
  let params = Setup.getParams setup
  simulation <- Pure.create
    { alphaMin: params.alphaMin
    , alphaDecay: params.alphaDecay
    , alphaTarget: params.alphaTarget
    , velocityRetention: params.velocityRetention
    }
  Pure.setNodes (copy <$> nodes) simulation
  Pure.setLinks links simulation
  Setup.applySetup setup simulation
  pure (Machine { simulation, alphaMin: params.alphaMin })

-- | One tick; the nodes where it left them.
step :: forall fields. Machine fields -> Effect (Array (SimulationNode fields))
step (Machine machine) = do
  void $ Pure.tick machine.simulation
  current machine.simulation

-- | Has the run cooled to its end?
finished :: forall fields. Machine fields -> Effect Boolean
finished (Machine machine) = (_ < machine.alphaMin) <$> Pure.getAlpha machine.simulation

alpha :: forall fields. Machine fields -> Effect Number
alpha (Machine machine) = Pure.getAlpha machine.simulation

-- | The simulation as it is now: alpha, the random source, every node's
-- | place, velocity and pins.
snapshot :: forall fields. Machine fields -> Effect Snapshot
snapshot (Machine machine) = do
  now <- Pure.getAlpha machine.simulation
  nodes <- Pure.getNodes machine.simulation
  Snapshot.take machine.simulation.random now nodes

-- | Put the simulation back as a snapshot recorded it; the nodes as they
-- | then are. The ticks that follow are the ticks that followed the snapshot.
restore :: forall fields. Snapshot -> Machine fields -> Effect (Array (SimulationNode fields))
restore recorded (Machine machine) = do
  nodes <- Pure.getNodes machine.simulation
  then_ <- Snapshot.restore recorded machine.simulation.random nodes
  Ref.write then_ machine.simulation.alpha
  Ref.write then_ machine.simulation.prevAlpha
  current machine.simulation

-- | Hold the node at this place in the simulation's nodes where it is
-- | (`Just` its place), or let it go (`Nothing`).
pin :: forall fields. Int -> Maybe { x :: Number, y :: Number } -> Machine fields -> Effect Unit
pin place at (Machine machine) = do
  nodes <- Pure.getNodes machine.simulation
  for_ (Array.index nodes place) \node -> case at of
    Just point -> runEffectFn3 writePin node (notNull point.x) (notNull point.y)
    Nothing -> runEffectFn3 writePin node null null

current :: forall fields. Simulation fields () -> Effect (Array (SimulationNode fields))
current simulation = map copy <$> Pure.getNodes simulation

-- | A new record with the same fields. (A record update copies.)
copy :: forall fields. SimulationNode fields -> SimulationNode fields
copy node = node { x = node.x }

-- | The two kernels behind one handle.
-- |
-- | `hylograph-simulation` runs one `Setup` on either kernel, but through two
-- | different sets of functions: the PureScript kernel is
-- | `Hylograph.ForceEngine.Simulation` with `Hylograph.ForceEngine.Setup`,
-- | the Rust one is `Hylograph.ForceEngine.Setup.WASM`. This is the least a
-- | demo needs of both, as one record.
-- |
-- | WORKAROUNDS (hylograph-simulation), all on the PureScript kernel's side:
-- | - it has no `step`, `settled`, `maxDisplacement` or `maxAcceleration`;
-- |   the measures are `hylograph-forces-pure`'s own (`Measure`), called
-- |   here round each tick;
-- | - its `applySetup` uses a setup's `alpha` and ignores the rest of its
-- |   parameters, which are fixed when the simulation is created, so the
-- |   simulation is created from the setup's parameters here;
-- | - it writes into the node records it is given and hands the same records
-- |   back after every tick, so nodes are copied on the way in and out.
module Labels.Engine
  ( Kernel(..)
  , allKernels
  , kernelLabel
  , Engine
  , start
  ) where

import Prelude

import Effect (Effect)
import Hylograph.ForceEngine.Setup (Setup)
import Hylograph.ForceEngine.Setup as Setup
import Hylograph.ForceEngine.Setup.WASM as Wasm
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.ForceEngine.Simulation as Pure
import Hylograph.Forces.Pure.Measure as Measure

data Kernel
  = PureScript
  | Rust

derive instance Eq Kernel

allKernels :: Array Kernel
allKernels = [ PureScript, Rust ]

kernelLabel :: Kernel -> String
kernelLabel = case _ of
  PureScript -> "PureScript kernel"
  Rust -> "Rust kernel (WASM)"

-- | A running simulation, whichever kernel runs it.
type Engine fields =
  { kernel :: Kernel
  , step :: Effect (Array (SimulationNode fields)) -- one tick; the nodes where it left them
  , settled :: { step :: Number, acceleration :: Number } -> Effect Boolean
  , apply :: Setup (SimulationNode fields) -> Effect Unit -- other forces, same nodes, still moving
  , stop :: Effect Unit -- give back what the kernel holds
  }

-- | A simulation of these nodes (no links) under this setup.
start :: forall fields. Kernel -> Setup (SimulationNode fields) -> Array (SimulationNode fields) -> Effect (Engine fields)
start kernel setup nodes = case kernel of
  Rust -> do
    simulation <- Wasm.create nodes []
    Wasm.applySetup setup simulation
    pure
      { kernel
      , step: Wasm.step simulation
      , settled: \tolerance -> Wasm.settled tolerance simulation
      , apply: \next -> Wasm.applySetup next simulation
      , stop: Wasm.free simulation
      }
  PureScript -> do
    let params = Setup.getParams setup
    simulation <- Pure.create
      { alphaMin: params.alphaMin
      , alphaDecay: params.alphaDecay
      , alphaTarget: params.alphaTarget
      , velocityRetention: params.velocityRetention
      }
    Pure.setNodes (copy <$> nodes) simulation
    Setup.applySetup setup simulation
    watch <- Measure.watch
    pure
      { kernel
      , step: do
          own <- Pure.getNodes simulation
          Measure.before watch own
          void $ Pure.tick simulation
          Measure.after watch own
          pure (copy <$> own)
      , settled: \tolerance -> Measure.settled tolerance watch
      , apply: \next -> Setup.applySetup next simulation
      , stop: pure unit
      }

-- | A new record with the same fields. (A record update copies.)
copy :: forall fields. SimulationNode fields -> SimulationNode fields
copy node = node { x = node.x }

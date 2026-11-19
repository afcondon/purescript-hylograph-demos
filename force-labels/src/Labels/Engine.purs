-- | Which kernel runs the simulation. The simulation itself is
-- | `Hylograph.Simulation.Stepped`, one handle over both.
module Labels.Engine
  ( Kernel(..)
  , allKernels
  , kernelLabel
  , engineOf
  ) where

import Prelude

import Hylograph.Simulation.Stepped as Stepped

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

engineOf :: Kernel -> Stepped.Engine
engineOf = case _ of
  PureScript -> Stepped.Default
  Rust -> Stepped.Rust

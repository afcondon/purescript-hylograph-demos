-- | Anscombe's Quartet Data
-- |
-- | Four datasets with identical statistical properties:
-- | - Mean of x: 9
-- | - Mean of y: 7.50
-- | - Variance of x: 11
-- | - Variance of y: 4.12
-- | - Correlation: 0.816
-- | - Linear regression: y = 3.00 + 0.500x
-- |
-- | Yet visually (and aurally) they are completely different.
module Anscombe.Data
  ( Point
  , Dataset
  , datasetI
  , datasetII
  , datasetIII
  , datasetIV
  , allDatasets
  ) where

import Prelude
import Data.Array as Array

type Point = { x :: Number, y :: Number }
type Dataset = { name :: String, numeral :: String, points :: Array Point }

-- | Dataset I: Normal linear relationship with scatter
datasetI :: Dataset
datasetI =
  { name: "I"
  , numeral: "I"
  , points: Array.sortBy (\a b -> compare a.x b.x)
      [ { x: 10.0, y: 8.04 }
      , { x: 8.0, y: 6.95 }
      , { x: 13.0, y: 7.58 }
      , { x: 9.0, y: 8.81 }
      , { x: 11.0, y: 8.33 }
      , { x: 14.0, y: 9.96 }
      , { x: 6.0, y: 7.24 }
      , { x: 4.0, y: 4.26 }
      , { x: 12.0, y: 10.84 }
      , { x: 7.0, y: 4.82 }
      , { x: 5.0, y: 5.68 }
      ]
  }

-- | Dataset II: Curved/parabolic relationship
datasetII :: Dataset
datasetII =
  { name: "II"
  , numeral: "II"
  , points: Array.sortBy (\a b -> compare a.x b.x)
      [ { x: 10.0, y: 9.14 }
      , { x: 8.0, y: 8.14 }
      , { x: 13.0, y: 8.74 }
      , { x: 9.0, y: 8.77 }
      , { x: 11.0, y: 9.26 }
      , { x: 14.0, y: 8.10 }
      , { x: 6.0, y: 6.13 }
      , { x: 4.0, y: 3.10 }
      , { x: 12.0, y: 9.13 }
      , { x: 7.0, y: 7.26 }
      , { x: 5.0, y: 4.74 }
      ]
  }

-- | Dataset III: Perfect linear except one outlier
datasetIII :: Dataset
datasetIII =
  { name: "III"
  , numeral: "III"
  , points: Array.sortBy (\a b -> compare a.x b.x)
      [ { x: 10.0, y: 7.46 }
      , { x: 8.0, y: 6.77 }
      , { x: 13.0, y: 12.74 }
      , { x: 9.0, y: 7.11 }
      , { x: 11.0, y: 7.81 }
      , { x: 14.0, y: 8.84 }
      , { x: 6.0, y: 6.08 }
      , { x: 4.0, y: 5.39 }
      , { x: 12.0, y: 8.15 }
      , { x: 7.0, y: 6.42 }
      , { x: 5.0, y: 5.73 }
      ]
  }

-- | Dataset IV: All points at x=8 except one outlier at x=19
datasetIV :: Dataset
datasetIV =
  { name: "IV"
  , numeral: "IV"
  , points: Array.sortBy (\a b -> compare a.x b.x)
      [ { x: 8.0, y: 6.58 }
      , { x: 8.0, y: 5.76 }
      , { x: 8.0, y: 7.71 }
      , { x: 8.0, y: 8.84 }
      , { x: 8.0, y: 8.47 }
      , { x: 8.0, y: 7.04 }
      , { x: 8.0, y: 5.25 }
      , { x: 19.0, y: 12.50 }
      , { x: 8.0, y: 5.56 }
      , { x: 8.0, y: 7.91 }
      , { x: 8.0, y: 6.89 }
      ]
  }

allDatasets :: Array Dataset
allDatasets = [ datasetI, datasetII, datasetIII, datasetIV ]

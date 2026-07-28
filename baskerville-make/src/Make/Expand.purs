-- | Static variable expansion — the piece arid-keystone's parser left
-- | out. `=` and `:=` are both treated textually (no evaluation-time
-- | semantics matter for a static read); automatic variables and
-- | function calls expand to "", so names containing them drop out of
-- | the model at the word-split.
module Make.Expand
  ( expandExpr
  , expandNames
  , render
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Make.Ast (AutomaticVariable(..), Expression(..), Variable)

-- | Expand an expression to flat text, resolving variable references
-- | recursively (depth-capped against definition cycles).
expandExpr :: Map String Variable -> Expression -> String
expandExpr vars = go 16
  where
  go :: Int -> Expression -> String
  go depth = case _ of
    Literal s -> s
    VarRef name
      | depth <= 0 -> ""
      | otherwise -> case Map.lookup name vars of
          Just v -> go (depth - 1) v.value
          Nothing -> ""
    AutoVar _ -> ""
    FunctionCall _ _ -> ""
    Concat parts -> foldMap (go depth) parts

-- | Expand and whitespace-split: `$(SRCS)` becomes its member names.
expandNames :: Map String Variable -> Expression -> Array String
expandNames vars expr =
  Array.filter (_ /= "")
    (String.split (Pattern " ") collapsed)
  where
  collapsed = String.replaceAll (Pattern "\t") (Replacement " ") (expandExpr vars expr)

-- | Pretty-print an expression back to Makefile-flavoured text —
-- | for showing recipes in the proof panel, not for semantics.
render :: Expression -> String
render = case _ of
  Literal s -> s
  VarRef n -> "$(" <> n <> ")"
  AutoVar v -> renderAuto v
  FunctionCall n args -> "$(" <> n <> " " <> String.joinWith "," (render <$> args) <> ")"
  Concat parts -> foldMap render parts
  where
  renderAuto = case _ of
    TargetVar -> "$@"
    FirstPrereq -> "$<"
    AllPrereqs -> "$^"
    AllPrereqsDup -> "$+"
    NewerPrereqs -> "$?"
    Stem -> "$*"

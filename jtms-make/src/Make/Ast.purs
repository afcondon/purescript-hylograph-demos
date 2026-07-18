-- | The Makefile AST — a trimmed adaptation of psd3-arid-keystone's
-- | Makefile.Types. What survives is exactly what the parser emits.
-- | What didn't make the cut: source locations (never populated
-- | upstream), the runtime-status layer, and the Sankey-specific types
-- | (jtms-make derives its diagram from the KB, not the AST).
module Make.Ast where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)

-- | A parsed Makefile.
type Makefile =
  { variables :: Map String Variable
  , rules :: Array Rule
  , includes :: Array String
  , phonyTargets :: Set String
  , defaultTarget :: Maybe String
  , comments :: Array Comment
  }

-- | Variable definition; the value stays unexpanded.
type Variable =
  { name :: String
  , value :: Expression
  , flavor :: VariableFlavor
  }

data VariableFlavor
  = RecursiveVar -- =  (expanded when used)
  | SimpleVar -- := (expanded when defined)
  | ConditionalVar -- ?= (set if not already set)
  | AppendVar -- += (append to existing)

derive instance Eq VariableFlavor

-- | A make rule (explicit or pattern).
type Rule =
  { targets :: Array Target
  , prerequisites :: Prerequisites
  , recipe :: Maybe Recipe
  , isPatternRule :: Boolean
  , docComment :: Maybe String
  }

type Target = { name :: Expression, targetType :: TargetType }

data TargetType
  = FileTarget String
  | PatternTarget String -- contains % (stem pattern)

derive instance Eq TargetType

-- | Prerequisites; order-only (after |) kept separate at parse time,
-- | folded into normal by the model.
type Prerequisites =
  { normal :: Array Expression
  , orderOnly :: Array Expression
  }

type Recipe = { commands :: Array Command }

type Command = { text :: Expression, prefix :: CommandPrefix }

type CommandPrefix =
  { silent :: Boolean -- @ prefix
  , ignoreError :: Boolean -- - prefix
  , recursive :: Boolean -- + prefix
  }

-- | Expression that may contain variables, functions, literals.
data Expression
  = Literal String
  | VarRef String -- $(VAR) or ${VAR}
  | AutoVar AutomaticVariable -- $@, $<, …
  | FunctionCall String (Array Expression) -- $(func args…), name kept raw
  | Concat (Array Expression)

derive instance Eq Expression

data AutomaticVariable
  = TargetVar -- $@
  | FirstPrereq -- $<
  | AllPrereqs -- $^
  | AllPrereqsDup -- $+
  | NewerPrereqs -- $?
  | Stem -- $*

derive instance Eq AutomaticVariable

-- | Conditional block; the model takes the then-branch.
type Conditional =
  { condition :: Condition
  , thenBranch :: Array MakefileElement
  , elseBranch :: Array MakefileElement
  }

data Condition
  = IfEq Expression Expression
  | IfNeq Expression Expression
  | IfDef String
  | IfNdef String

derive instance Eq Condition

data MakefileElement
  = RuleElement Rule
  | VariableElement Variable
  | ConditionalElement Conditional
  | IncludeElement String
  | CommentElement Comment
  | DirectiveElement Directive

derive instance Eq MakefileElement

data Directive
  = Phony (Array String)
  | Suffixes (Array String)
  | Default (Array String)
  | Precious (Array String)
  | Export (Maybe String)

derive instance Eq Directive

type Comment = { text :: String }

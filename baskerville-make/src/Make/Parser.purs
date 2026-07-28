-- | Parser combinators for GNU Make syntax — an adaptation of
-- | psd3-arid-keystone's Makefile.Parser, with two additions that
-- | parser lacked: a preprocessor that joins backslash line
-- | continuations, and conditional blocks flattened to their
-- | then-branch. Source locations are gone (they were never populated
-- | upstream); function-call names are kept raw rather than mapped to
-- | a 40-constructor enum.
-- |
-- | Context-sensitivity is the whole game here: what a literal may
-- | contain depends on where it sits (target, prerequisite, recipe,
-- | variable value, function argument), so each position has its own
-- | literal parser.
module Make.Parser
  ( parseMakefileText
  , parseMakefile
  , preprocess
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple.Nested ((/\))
import Parsing (Parser, ParseError, runParser)
import Parsing.Combinators (choice, many, many1, notFollowedBy, sepBy, sepBy1, try)
import Parsing.String (char, eof, satisfy, string)
import Parsing.String.Basic (alphaNum)

import Make.Ast (AutomaticVariable(..), Command, CommandPrefix, Comment, Condition(..), Conditional, Directive(..), Expression(..), Makefile, MakefileElement(..), Prerequisites, Recipe, Rule, Target, TargetType(..), Variable, VariableFlavor(..))

-- =============================================================================
-- Entry points
-- =============================================================================

-- | Preprocess (join continuations, strip CR) and parse.
parseMakefileText :: String -> Either ParseError Makefile
parseMakefileText = parseMakefile <<< preprocess

-- | Parse already-preprocessed Makefile text.
parseMakefile :: String -> Either ParseError Makefile
parseMakefile contents = runParser contents makefileP

-- | Join backslash line continuations (the continuation line's leading
-- | whitespace collapses to a single space) and drop carriage returns.
preprocess :: String -> String
preprocess text =
  String.joinWith "\n" (finish (foldl step { done: [], pending: Nothing } lines))
  where
  lines = String.split (Pattern "\n") (String.replaceAll (Pattern "\r") (Replacement "") text)

  step acc line =
    let
      joined = case acc.pending of
        Nothing -> line
        Just prefix -> prefix <> " " <> trimStart line
    in
      case String.stripSuffix (Pattern "\\") joined of
        Just body -> acc { pending = Just body }
        Nothing -> { done: Array.snoc acc.done joined, pending: Nothing }

  finish acc = case acc.pending of
    Just body -> Array.snoc acc.done body
    Nothing -> acc.done

  trimStart = SCU.dropWhile \c -> c == ' ' || c == '\t'

-- =============================================================================
-- Top level
-- =============================================================================

makefileP :: Parser String Makefile
makefileP = do
  elements <- Array.fromFoldable <$> many elementP
  eof
  pure (buildMakefile (flattenConditionals elements))

-- | Conditionals contribute their then-branch — enough for the static
-- | reading this demo needs.
flattenConditionals :: Array MakefileElement -> Array MakefileElement
flattenConditionals = Array.concatMap case _ of
  ConditionalElement c -> flattenConditionals c.thenBranch
  el -> [ el ]

buildMakefile :: Array MakefileElement -> Makefile
buildMakefile elements =
  { variables: Map.fromFoldable vars
  , rules
  , includes
  , phonyTargets: Set.fromFoldable phonies
  , defaultTarget: findDefault rules
  , comments
  }
  where
  vars = Array.mapMaybe getVar elements
  rules = Array.mapMaybe getRule elements
  includes = Array.mapMaybe getInclude elements
  phonies = Array.concatMap getPhony elements
  comments = Array.mapMaybe getComment elements

  getVar = case _ of
    VariableElement v -> Just (v.name /\ v)
    _ -> Nothing

  getRule = case _ of
    RuleElement r -> Just r
    _ -> Nothing

  getInclude = case _ of
    IncludeElement path -> Just path
    _ -> Nothing

  getPhony = case _ of
    DirectiveElement (Phony targets) -> targets
    _ -> []

  getComment = case _ of
    CommentElement c -> Just c
    _ -> Nothing

  findDefault rs = case Array.find (not <<< _.isPatternRule) rs of
    Just r -> case Array.head r.targets of
      Just t -> case t.targetType of
        FileTarget name -> Just name
        _ -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing

elementP :: Parser String MakefileElement
elementP = do
  skipEmptyLines
  choice
    [ try (DirectiveElement <$> directiveP)
    , try (ConditionalElement <$> conditionalP)
    , try (IncludeElement <$> includeP)
    , try (VariableElement <$> variableP)
    , try (RuleElement <$> ruleP)
    , CommentElement <$> commentP
    ]

-- =============================================================================
-- Rules
-- =============================================================================

ruleP :: Parser String Rule
ruleP = do
  docComment <- (Just <$> try docCommentP) <|> pure Nothing
  targets <- targetsP
  _ <- ruleSeparatorP
  prereqs <- prerequisitesP
  _ <- eolP
  recipe <- (Just <$> try recipeP) <|> pure Nothing
  pure
    { targets
    , prerequisites: prereqs
    , recipe
    , isPatternRule: Array.any isPattern targets
    , docComment
    }
  where
  isPattern t = case t.targetType of
    PatternTarget _ -> true
    _ -> false

targetsP :: Parser String (Array Target)
targetsP = Array.fromFoldable <$> sepBy1 targetP skipHSpace

targetP :: Parser String Target
targetP = do
  expr <- targetExpressionP
  pure { name: expr, targetType: classifyTarget expr }
  where
  classifyTarget = case _ of
    Literal s
      | SCU.contains (Pattern "%") s -> PatternTarget s
      | otherwise -> FileTarget s
    _ -> FileTarget "" -- variables need expansion

targetExpressionP :: Parser String Expression
targetExpressionP = defer \_ -> do
  parts <- many1 targetExprPartP
  pure case Array.fromFoldable parts of
    [ single ] -> single
    multiple -> Concat multiple

targetExprPartP :: Parser String Expression
targetExprPartP = defer \_ -> choice
  [ try autoVarP
  , try varRefP
  , try functionCallP
  , targetLiteralP
  ]

-- | Literal text in target position (stops at space, $, \n, \t, :, #).
targetLiteralP :: Parser String Expression
targetLiteralP = do
  chars <- many1 (satisfy \c -> c /= '$' && c /= '\n' && c /= '\t' && c /= ':' && c /= '#' && c /= ' ')
  pure (Literal (SCU.fromCharArray (Array.fromFoldable chars)))

-- | : or :: — double-colon rules are treated as ordinary here.
ruleSeparatorP :: Parser String String
ruleSeparatorP = do
  skipHSpace
  try (string "::") <|> string ":"

prerequisitesP :: Parser String Prerequisites
prerequisitesP = do
  skipHSpace
  normal <- Array.fromFoldable <$> many (try prereqP)
  orderOnly <- (Just <$> try orderOnlyP) <|> pure Nothing
  pure { normal, orderOnly: maybe [] Array.fromFoldable orderOnly }
  where
  prereqP = do
    notFollowedBy (char '|')
    notFollowedBy eolP
    expr <- prereqExpressionP
    skipHSpace
    pure expr

  orderOnlyP = do
    _ <- char '|'
    skipHSpace
    many prereqP

prereqExpressionP :: Parser String Expression
prereqExpressionP = defer \_ -> do
  parts <- many1 prereqExprPartP
  pure case Array.fromFoldable parts of
    [ single ] -> single
    multiple -> Concat multiple

prereqExprPartP :: Parser String Expression
prereqExprPartP = defer \_ -> choice
  [ try autoVarP
  , try varRefP
  , try functionCallP
  , prereqLiteralP
  ]

-- | Literal text in prerequisite position (also stops at |).
prereqLiteralP :: Parser String Expression
prereqLiteralP = do
  chars <- many1 (satisfy \c -> c /= '$' && c /= '\n' && c /= '\t' && c /= ':' && c /= '#' && c /= ' ' && c /= '|')
  pure (Literal (SCU.fromCharArray (Array.fromFoldable chars)))

-- =============================================================================
-- Recipes
-- =============================================================================

recipeP :: Parser String Recipe
recipeP = do
  commands <- Array.fromFoldable <$> many1 commandLineP
  pure { commands }

-- | A command line must start with a tab.
commandLineP :: Parser String Command
commandLineP = do
  _ <- char '\t'
  prefix <- commandPrefixP
  text <- recipeExpressionP
  _ <- eolP
  pure { text, prefix }

recipeExpressionP :: Parser String Expression
recipeExpressionP = defer \_ -> do
  parts <- many1 recipeExprPartP
  pure case Array.fromFoldable parts of
    [ single ] -> single
    multiple -> Concat multiple

recipeExprPartP :: Parser String Expression
recipeExprPartP = defer \_ -> choice
  [ try dollarLiteralP -- $$ -> literal $
  , try autoVarP
  , try varRefP
  , try functionCallP
  , recipeLiteralP
  ]

dollarLiteralP :: Parser String Expression
dollarLiteralP = do
  _ <- string "$$"
  pure (Literal "$")

-- | Literal text in recipe position (allows : and #).
recipeLiteralP :: Parser String Expression
recipeLiteralP = do
  chars <- many1 (satisfy \c -> c /= '$' && c /= '\n' && c /= '\t')
  pure (Literal (SCU.fromCharArray (Array.fromFoldable chars)))

commandPrefixP :: Parser String CommandPrefix
commandPrefixP = do
  chars <- many (satisfy \c -> c == '@' || c == '-' || c == '+')
  let charArray = Array.fromFoldable chars
  pure
    { silent: Array.elem '@' charArray
    , ignoreError: Array.elem '-' charArray
    , recursive: Array.elem '+' charArray
    }

-- =============================================================================
-- Variables
-- =============================================================================

variableP :: Parser String Variable
variableP = do
  name <- identifierP
  skipHSpace
  flavor <- flavorP
  skipHSpace
  value <- varValueExpressionP
  _ <- eolP
  pure { name, value, flavor }

varValueExpressionP :: Parser String Expression
varValueExpressionP = defer \_ -> do
  parts <- many1 varValueExprPartP
  pure case Array.fromFoldable parts of
    [ single ] -> single
    multiple -> Concat multiple

varValueExprPartP :: Parser String Expression
varValueExprPartP = defer \_ -> choice
  [ try autoVarP
  , try varRefP
  , try functionCallP
  , varValueLiteralP
  ]

-- | Literal text in variable-value position (allows colons).
varValueLiteralP :: Parser String Expression
varValueLiteralP = do
  chars <- many1 (satisfy \c -> c /= '$' && c /= '\n' && c /= '\t' && c /= '#')
  pure (Literal (SCU.fromCharArray (Array.fromFoldable chars)))

flavorP :: Parser String VariableFlavor
flavorP = choice
  [ try (string ":=") $> SimpleVar
  , try (string "?=") $> ConditionalVar
  , try (string "+=") $> AppendVar
  , string "=" $> RecursiveVar
  ]

-- =============================================================================
-- Shared expression parts
-- =============================================================================

autoVarP :: Parser String Expression
autoVarP = do
  _ <- char '$'
  c <- satisfy \c -> c == '@' || c == '<' || c == '^' || c == '+' || c == '?' || c == '*'
  pure $ AutoVar case c of
    '@' -> TargetVar
    '<' -> FirstPrereq
    '^' -> AllPrereqs
    '+' -> AllPrereqsDup
    '?' -> NewerPrereqs
    _ -> Stem

varRefP :: Parser String Expression
varRefP = do
  _ <- char '$'
  opener <- char '(' <|> char '{'
  let closer = if opener == '(' then ')' else '}'
  name <- many1 (satisfy \c -> c /= closer && c /= ':' && c /= ' ')
  _ <- char closer
  pure (VarRef (SCU.fromCharArray (Array.fromFoldable name)))

functionCallP :: Parser String Expression
functionCallP = do
  _ <- string "$("
  funcName <- identifierP
  skipHSpace
  args <- Array.fromFoldable <$> sepBy (defer \_ -> funcArgExpressionP) (char ',')
  _ <- char ')'
  pure (FunctionCall funcName args)

funcArgExpressionP :: Parser String Expression
funcArgExpressionP = defer \_ -> do
  parts <- many1 funcArgExprPartP
  pure case Array.fromFoldable parts of
    [ single ] -> single
    multiple -> Concat multiple

funcArgExprPartP :: Parser String Expression
funcArgExprPartP = defer \_ -> choice
  [ try autoVarP
  , try varRefP
  , try functionCallP
  , funcArgLiteralP
  ]

-- | Literal text in function-argument position (stops at , and )).
funcArgLiteralP :: Parser String Expression
funcArgLiteralP = do
  chars <- many1 (satisfy \c -> c /= '$' && c /= '\n' && c /= '\t' && c /= ':' && c /= '#' && c /= ')' && c /= ',')
  pure (Literal (SCU.fromCharArray (Array.fromFoldable chars)))

-- =============================================================================
-- Directives
-- =============================================================================

directiveP :: Parser String Directive
directiveP = choice
  [ try phonyP
  , try suffixesP
  , try defaultP
  , try preciousP
  , try exportP
  ]

phonyP :: Parser String Directive
phonyP = do
  _ <- string ".PHONY"
  _ <- char ':'
  skipHSpace
  targets <- Array.fromFoldable <$> sepBy identifierP skipHSpace
  _ <- eolP
  pure (Phony targets)

suffixesP :: Parser String Directive
suffixesP = do
  _ <- string ".SUFFIXES"
  _ <- char ':'
  skipHSpace
  suffixes <- Array.fromFoldable <$> sepBy identifierP skipHSpace
  _ <- eolP
  pure (Suffixes suffixes)

defaultP :: Parser String Directive
defaultP = do
  _ <- string ".DEFAULT"
  _ <- char ':'
  skipHSpace
  targets <- Array.fromFoldable <$> sepBy identifierP skipHSpace
  _ <- eolP
  pure (Default targets)

preciousP :: Parser String Directive
preciousP = do
  _ <- string ".PRECIOUS"
  _ <- char ':'
  skipHSpace
  targets <- Array.fromFoldable <$> sepBy identifierP skipHSpace
  _ <- eolP
  pure (Precious targets)

exportP :: Parser String Directive
exportP = do
  _ <- string "export"
  skipHSpace
  var <- (Just <$> try identifierP) <|> pure Nothing
  _ <- eolP
  pure (Export var)

-- =============================================================================
-- Conditionals
-- =============================================================================

conditionalP :: Parser String Conditional
conditionalP = do
  condition <- conditionP
  _ <- eolP
  thenBranch <- Array.fromFoldable <$> many elementP
  elseBranch <- (Just <$> try elseP) <|> pure Nothing
  _ <- string "endif"
  _ <- eolP
  pure
    { condition
    , thenBranch
    , elseBranch: maybe [] Array.fromFoldable elseBranch
    }
  where
  elseP = do
    _ <- string "else"
    _ <- eolP
    many elementP

conditionP :: Parser String Condition
conditionP = choice
  [ try ifeqP
  , try ifneqP
  , try ifdefP
  , ifndefP
  ]

ifeqP :: Parser String Condition
ifeqP = do
  _ <- string "ifeq"
  skipHSpace
  _ <- char '('
  a <- condArgExprP
  _ <- char ','
  b <- condArgExprP
  _ <- char ')'
  pure (IfEq a b)

ifneqP :: Parser String Condition
ifneqP = do
  _ <- string "ifneq"
  skipHSpace
  _ <- char '('
  a <- condArgExprP
  _ <- char ','
  b <- condArgExprP
  _ <- char ')'
  pure (IfNeq a b)

condArgExprP :: Parser String Expression
condArgExprP = defer \_ -> do
  parts <- many funcArgExprPartP
  pure case Array.fromFoldable parts of
    [] -> Literal ""
    [ single ] -> single
    multiple -> Concat multiple

ifdefP :: Parser String Condition
ifdefP = do
  _ <- string "ifdef"
  skipHSpace
  name <- identifierP
  pure (IfDef name)

ifndefP :: Parser String Condition
ifndefP = do
  _ <- string "ifndef"
  skipHSpace
  name <- identifierP
  pure (IfNdef name)

-- =============================================================================
-- Includes and comments
-- =============================================================================

includeP :: Parser String String
includeP = do
  _ <- string "include" <|> string "-include" <|> string "sinclude"
  skipHSpace
  path <- many1 (satisfy \c -> c /= '\n' && c /= '#')
  _ <- eolP
  pure (SCU.fromCharArray (Array.fromFoldable path))

commentP :: Parser String Comment
commentP = do
  _ <- char '#'
  text <- many (satisfy \c -> c /= '\n')
  _ <- eolP
  pure { text: SCU.fromCharArray (Array.fromFoldable text) }

-- | Documentation comment (## prefix, immediately before a rule).
docCommentP :: Parser String String
docCommentP = do
  _ <- string "##"
  text <- many (satisfy \c -> c /= '\n')
  _ <- eolP
  pure (SCU.fromCharArray (Array.fromFoldable text))

-- =============================================================================
-- Utilities
-- =============================================================================

identifierP :: Parser String String
identifierP = do
  chars <- many1 (alphaNum <|> char '_' <|> char '-' <|> char '.')
  pure (SCU.fromCharArray (Array.fromFoldable chars))

skipHSpace :: Parser String Unit
skipHSpace = void (many (satisfy \c -> c == ' ' || c == '\t'))

eolP :: Parser String Unit
eolP = void (char '\n') <|> eof

-- | Skip empty lines and single-# comments (but not ## doc comments).
skipEmptyLines :: Parser String Unit
skipEmptyLines = void (many (try emptyLineP))
  where
  emptyLineP = do
    skipHSpace
    _ <- (Just <$> try commentPart) <|> pure Nothing
    void (char '\n')

  commentPart = do
    _ <- char '#'
    notFollowedBy (char '#')
    void (many (satisfy \c -> c /= '\n'))

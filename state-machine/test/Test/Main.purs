-- | Phase 1 acceptance tests for `docs/kb/plans/the-same-machine.md`.
-- |
-- | The headline test is `carRadioNeedsNoPureScript`. Everything else here
-- | supports it. If a machine can only be added by writing PureScript then the
-- | thesis has failed and no amount of round-tripping rescues it.
module Test.Main where

import Prelude

import Data.Array (all, elem, filter, length, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Glassbox.Codec.JSON (decodeSpec, encodeSpec, parseSpec)
import Glassbox.Describe (defaultOptions, describe)
import Glassbox.Run (dueAt, resolve, setConfig, setFact, step, worldFrom)
import Glassbox.Spec
  ( ConfigId(..)
  , EventId(..)
  , FactId(..)
  , Outcome(..)
  , RefusalId(..)
  , Spec
  , StateId(..)
  , Value(..)
  , eventIds
  , stateIds
  , userEvents
  )
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile)
import Node.Process (exit', setExitCode)

-- =============================================================================
-- Harness — matches the plain-Effect style used by hylograph-layout's tests
-- =============================================================================

check :: String -> Boolean -> Effect Boolean
check name ok = do
  log ((if ok then "  ok   " else "  FAIL ") <> name)
  pure ok

section :: String -> Effect Unit
section name = log ("\n" <> name)

-- | spago may run tests from the package directory or from the workspace root.
readMachine :: String -> Effect String
readMachine name = do
  let local = "public/machines/" <> name
  let fromRoot = "state-machine/public/machines/" <> name
  here <- exists local
  readTextFile UTF8 (if here then local else fromRoot)

loadOrDie :: String -> Effect Spec
loadOrDie name = do
  text <- readMachine name
  case parseSpec text of
    Left err -> do
      log ("  FATAL could not parse " <> name <> ": " <> err)
      exit' 1
    Right spec -> pure spec

-- =============================================================================
-- The acceptance test
-- =============================================================================

-- | **The phase 1 acceptance criterion.**
-- |
-- | `car-radio.json` was authored as a file. No constructor names its states, no
-- | module was recompiled to add it, and nothing in this repository knows what a
-- | car radio is. It must nonetheless load, run, refuse, and draw.
carRadioNeedsNoPureScript :: Effect (Array Boolean)
carRadioNeedsNoPureScript = do
  section "A machine that exists only as a file"
  spec <- loadOrDie "car-radio.json"
  let world = worldFrom spec

  a <- check "loads and reports its own identity"
    (spec.id == "car-radio" && spec.title == "Car radio")

  b <- check "declares five states and six events"
    (length (stateIds spec) == 5 && length (eventIds spec) == 6)

  -- off --power--> radio --seek--> seeking
  let Tuple s1 _ = step spec world spec.initial (EventId "power")
  let Tuple s2 _ = step spec world s1 (EventId "seek")
  c <- check "runs: off -power-> radio -seek-> seeking"
    (s1 == StateId "radio" && s2 == StateId "seeking")

  -- The seek gives up by itself after the configured interval.
  d <- check "seeking resolves by itself after seek-seconds"
    (dueAt spec world (StateId "seeking") 0.0 == Just { fires: EventId "giveup", at: 8.0 })

  -- A refusal carries a tag, not prose, and not silence.
  e <- check "ejecting with no disc refuses by tag"
    (resolve spec world (StateId "radio") (EventId "eject") == Refuse (RefusalId "no-disc"))

  -- Facts are supplied by the host and change what an event means.
  let loaded = setFact (FactId "disc-loaded") (VBoolean true) world
  f <- check "with a disc loaded, source reaches CD"
    (resolve spec loaded (StateId "radio") (EventId "source") == Move (StateId "cd"))
  g <- check "with no disc, the same press refuses"
    (resolve spec world (StateId "radio") (EventId "source") == Refuse (RefusalId "no-disc"))

  h <- check "only user events are offered as buttons"
    (not (elem (EventId "giveup") (userEvents spec)) && length (userEvents spec) == 5)

  pure [ a, b, c, d, e, f, g, h ]

-- =============================================================================
-- Config reshapes the machine
-- =============================================================================

-- | The mechanical test the format uses to sort config from parameter: *does the
-- | diagram change when you change it?* If this fails, the distinction is a
-- | judgement call again, and two runtimes have nothing to agree about.
configReshapesTheDiagram :: Effect (Array Boolean)
configReshapesTheDiagram = do
  section "Config reshapes the machine, and the drawing says so"

  radio <- loadOrDie "car-radio.json"
  let fitted = setFact (FactId "disc-loaded") (VBoolean true) (worldFrom radio)
  let stripped = setConfig (ConfigId "has-cd-slot") (VBoolean false) fitted

  a <- check "with a CD slot, something reaches the CD state"
    (hasInbound (describe defaultOptions fitted radio) "cd")
  b <- check "without one, the CD state is stranded"
    (not (hasInbound (describe defaultOptions stripped radio) "cd"))

  loop <- loadOrDie "loop.json"
  let withCountIn = worldFrom loop
  let noCountIn = setConfig (ConfigId "count-in-beats") (VNumber 0.0) withCountIn

  c <- check "with a count-in, record reaches Armed"
    (resolve loop withCountIn (StateId "empty") (EventId "record") == Move (StateId "armed"))
  d <- check "with none, record goes straight to Recording"
    (resolve loop noCountIn (StateId "empty") (EventId "record") == Move (StateId "recording"))
  e <- check "and Armed is then unreachable in the derived diagram"
    (not (hasInbound (describe defaultOptions noCountIn loop) "armed"))

  pure [ a, b, c, d, e ]
  where
  hasInbound machine target =
    not (null (filter (\t -> t.to == target && t.from /= target) machine.transitions))

-- =============================================================================
-- The round trip
-- =============================================================================

-- | Encoding and decoding must be inverses, and must preserve *behaviour* and
-- | not merely structure — so the check drives the whole (state x event) cross
-- | product through both copies and compares outcomes.
roundTrips :: Effect (Array Boolean)
roundTrips = do
  section "The round trip"
  for ["loop.json", "car-radio.json"] \name -> do
    spec <- loadOrDie name
    case decodeSpec (encodeSpec spec) of
      Left err -> check (name <> " survives encode/decode") false <* log ("    " <> err)
      Right back -> do
        let world = worldFrom spec
        let same = all (\(Tuple s e) -> resolve spec world s e == resolve back world s e) (crossProduct spec)
        let sameShape = stateIds spec == stateIds back && eventIds spec == eventIds back
        check (name <> " round-trips, identically on every (state x event)") (same && sameShape)

crossProduct :: Spec -> Array (Tuple StateId EventId)
crossProduct spec = do
  s <- stateIds spec
  e <- eventIds spec
  pure (Tuple s e)

-- =============================================================================
-- Behaviour parity with the PureScript machine this replaces
-- =============================================================================

-- | The table below is `Glassbox.Demo.Loop.transition` as it stood at commit
-- | b057e2b, before the machine became data. Keeping it here is what makes the
-- | rewrite a port rather than a rewrite that happens to compile.
loopParity :: Effect (Array Boolean)
loopParity = do
  section "Parity with the PureScript loop machine it replaces"
  spec <- loadOrDie "loop.json"
  let world = worldFrom spec
  let held = setFact (FactId "converter-held") (VBoolean true) world

  let
    expected =
      [ Tuple (Tuple "empty" "record") (Move (StateId "armed"))
      , Tuple (Tuple "empty" "play") (Refuse (RefusalId "nothing-yet"))
      , Tuple (Tuple "empty" "clear") Stay
      , Tuple (Tuple "empty" "stop") Stay
      , Tuple (Tuple "empty" "elapsed") Stay
      , Tuple (Tuple "armed" "elapsed") (Move (StateId "recording"))
      , Tuple (Tuple "armed" "record") (Move (StateId "empty"))
      , Tuple (Tuple "armed" "stop") (Move (StateId "empty"))
      , Tuple (Tuple "armed" "play") Stay
      , Tuple (Tuple "armed" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "recording" "record") (Move (StateId "playing"))
      , Tuple (Tuple "recording" "play") (Move (StateId "playing"))
      , Tuple (Tuple "recording" "stop") (Move (StateId "stopped"))
      , Tuple (Tuple "recording" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "recording" "elapsed") Stay
      , Tuple (Tuple "playing" "record") (Move (StateId "overdubbing"))
      , Tuple (Tuple "playing" "stop") (Move (StateId "stopped"))
      , Tuple (Tuple "playing" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "playing" "play") Stay
      , Tuple (Tuple "playing" "elapsed") Stay
      , Tuple (Tuple "overdubbing" "record") (Move (StateId "playing"))
      , Tuple (Tuple "overdubbing" "play") (Move (StateId "playing"))
      , Tuple (Tuple "overdubbing" "stop") (Move (StateId "stopped"))
      , Tuple (Tuple "overdubbing" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "overdubbing" "elapsed") Stay
      , Tuple (Tuple "stopped" "play") (Move (StateId "playing"))
      , Tuple (Tuple "stopped" "clear") (Move (StateId "empty"))
      , Tuple (Tuple "stopped" "record") (Refuse (RefusalId "overdub-in-play"))
      , Tuple (Tuple "stopped" "stop") Stay
      , Tuple (Tuple "stopped" "elapsed") Stay
      ]

  let
    wrong = filter
      (\(Tuple (Tuple s e) want) -> resolve spec world (StateId s) (EventId e) /= want)
      expected

  a <- check ("all " <> show (length expected) <> " (state, event) pairs match the old table")
    (null wrong)
  b <- check "the converter refusal still fires when another loop holds it"
    (resolve spec held (StateId "empty") (EventId "record") == Refuse (RefusalId "converter-busy"))
  c <- check "the count-in deadline is declared on Armed"
    (isJust (dueAt spec world (StateId "armed") 0.0))
  pure [ a, b, c ]

-- =============================================================================
-- The boundary refuses what it does not understand
-- =============================================================================

boundaryIsChecked :: Effect (Array Boolean)
boundaryIsChecked = do
  section "The boundary is checked, not trusted"
  a <- check "a future format version is refused outright"
    (isLeft (parseSpec """{"glassbox":99,"id":"x","initial":"a","states":[{"id":"a"}]}"""))
  b <- check "a malformed case is refused"
    (isLeft (parseSpec """{"glassbox":1,"id":"x","initial":"a","states":[{"id":"a"}],"rules":[{"from":"a","on":"e","cases":[{}]}]}"""))
  c <- check "text that is not JSON is refused"
    (isLeft (parseSpec "not json at all"))
  pure [ a, b, c ]
  where
  isLeft = case _ of
    Left _ -> true
    Right _ -> false

-- =============================================================================

main :: Effect Unit
main = do
  log "Glassbox — phase 1"
  groups <- sequence
    [ carRadioNeedsNoPureScript
    , configReshapesTheDiagram
    , roundTrips
    , loopParity
    , boundaryIsChecked
    ]
  let results = join groups
  let passed = length (filter identity results)
  let total = length results
  log ("\n" <> show passed <> "/" <> show total <> " passed")
  if passed == total then log "All tests passed!" else setExitCode 1

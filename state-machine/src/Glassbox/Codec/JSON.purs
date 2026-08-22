-- | Glassbox.Codec.JSON
-- |
-- | The wire format: one bidirectional codec, round-trip tested.
-- |
-- | This is the boundary the whole design turns on. A machine is data loaded at
-- | runtime, so this module is the single place where untyped bytes become typed
-- | values — and, per `docs/kb/plans/the-same-machine.md`, it is to be treated
-- | exactly as an FFI boundary is treated: narrow, explicit, versioned, checked
-- | rather than trusted, and loud on failure.
-- |
-- | Codecs are **values**, not type-class instances, per house style: a machine
-- | has one wire format that other languages must also implement, and an
-- | instance would tie that format to PureScript's notion of the type.
-- |
-- | ### Shape
-- |
-- | ```json
-- | { "glassbox": 1
-- | , "id": "car-radio"
-- | , "title": "Car radio"
-- | , "initial": "off"
-- | , "states":  [ { "id": "off", "label": "Off" } ]
-- | , "events":  [ { "id": "power", "label": "power", "source": "user" } ]
-- | , "config":  [ { "id": "band-count", "label": "Bands", "default": 2 } ]
-- | , "facts":   [ { "id": "disc-present", "label": "A disc is loaded" } ]
-- | , "refusals":[ { "id": "no-disc", "text": "there is no disc to play" } ]
-- | , "rules":   [ { "from": "off", "on": "power", "cases": [ { "to": "radio" } ] } ]
-- | , "deadlines": [ { "in": "seeking", "fires": "locked", "after": 3 } ]
-- | }
-- | ```
-- |
-- | A case is `{ "when": <guard>, "to" | "stay" | "refuse" }`, with `when`
-- | omitted meaning the catch-all. A guard is one of `{"fact":…}`,
-- | `{"config":…}`, a literal, `{"not":…}`, `{"all":[…]}`, `{"any":[…]}`, or a
-- | comparison `{"op":"gt","left":…,"right":…}`.
module Glassbox.Codec.JSON
  ( specCodec
  , decodeSpec
  , encodeSpec
  , parseSpec
  , printSpec
  , printSpecPretty
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser as JP
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Glassbox.Spec
  ( Case
  , CmpOp(..)
  , ConfigDecl
  , ConfigId(..)
  , Deadline
  , EventDecl
  , EventId(..)
  , EventSource(..)
  , Expr(..)
  , FactDecl
  , FactId(..)
  , Guard(..)
  , Outcome(..)
  , RefusalDecl
  , RefusalId(..)
  , Rule
  , Spec
  , StateDecl
  , StateId(..)
  , Value(..)
  , formatVersion
  )

-- =============================================================================
-- Entry points
-- =============================================================================

-- | Parse an artifact from text. The only function most callers need.
parseSpec :: String -> Either String Spec
parseSpec text = do
  json <- JP.jsonParser text
  decodeSpec json

decodeSpec :: Json -> Either String Spec
decodeSpec = lmap CA.printJsonDecodeError <<< Codec.decode specCodec

encodeSpec :: Spec -> Json
encodeSpec = Codec.encode specCodec

printSpec :: Spec -> String
printSpec = J.stringify <<< encodeSpec

foreign import stringifyPrettyImpl :: Json -> String

-- | The artifact as the decoder understood it, re-encoded and indented.
-- |
-- | Deliberately NOT the bytes that arrived. Showing the round trip is the
-- | point: if this differs from the file on disk, the codec lost something,
-- | and it can be seen rather than inferred from a passing test.
printSpecPretty :: Spec -> String
printSpecPretty = stringifyPrettyImpl <<< encodeSpec

-- =============================================================================
-- Small codecs
-- =============================================================================

stateIdCodec :: CA.JsonCodec StateId
stateIdCodec = CA.prismaticCodec "StateId" (Just <<< StateId) (\(StateId s) -> s) CA.string

eventIdCodec :: CA.JsonCodec EventId
eventIdCodec = CA.prismaticCodec "EventId" (Just <<< EventId) (\(EventId s) -> s) CA.string

refusalIdCodec :: CA.JsonCodec RefusalId
refusalIdCodec = CA.prismaticCodec "RefusalId" (Just <<< RefusalId) (\(RefusalId s) -> s) CA.string

configIdCodec :: CA.JsonCodec ConfigId
configIdCodec = CA.prismaticCodec "ConfigId" (Just <<< ConfigId) (\(ConfigId s) -> s) CA.string

factIdCodec :: CA.JsonCodec FactId
factIdCodec = CA.prismaticCodec "FactId" (Just <<< FactId) (\(FactId s) -> s) CA.string

-- | A value is written as its bare JSON self — `4`, `true`, `"fm"` — because an
-- | artifact is meant to be legible and hand-editable, and `{"number":4}` is
-- | neither.
valueCodec :: CA.JsonCodec Value
valueCodec = Codec.codec' decode encode
  where
  encode = case _ of
    VNumber n -> J.fromNumber n
    VBoolean b -> J.fromBoolean b
    VString s -> J.fromString s
  decode json =
    J.caseJson
      (const (Left (CA.TypeMismatch "number, boolean or string")))
      (Right <<< VBoolean)
      (Right <<< VNumber)
      (Right <<< VString)
      (const (Left (CA.TypeMismatch "number, boolean or string")))
      (const (Left (CA.TypeMismatch "number, boolean or string")))
      json

eventSourceCodec :: CA.JsonCodec EventSource
eventSourceCodec = CA.prismaticCodec "EventSource" from to CA.string
  where
  from = case _ of
    "user" -> Just FromUser
    "runtime" -> Just FromRuntime
    _ -> Nothing
  to = case _ of
    FromUser -> "user"
    FromRuntime -> "runtime"

cmpOpCodec :: CA.JsonCodec CmpOp
cmpOpCodec = CA.prismaticCodec "CmpOp" from to CA.string
  where
  from = case _ of
    "eq" -> Just OpEq
    "ne" -> Just OpNe
    "lt" -> Just OpLt
    "le" -> Just OpLe
    "gt" -> Just OpGt
    "ge" -> Just OpGe
    _ -> Nothing
  to = case _ of
    OpEq -> "eq"
    OpNe -> "ne"
    OpLt -> "lt"
    OpLe -> "le"
    OpGt -> "gt"
    OpGe -> "ge"

-- =============================================================================
-- Expressions and guards
-- =============================================================================

-- | `{"config": "count-in"}`, `{"fact": "converter-held"}`, or a bare literal.
exprCodec :: CA.JsonCodec Expr
exprCodec = Codec.codec' decode encode
  where
  encode = case _ of
    Lit v -> Codec.encode valueCodec v
    ConfigOf (ConfigId k) -> obj [ Tuple "config" (J.fromString k) ]
    FactOf (FactId k) -> obj [ Tuple "fact" (J.fromString k) ]
  decode json = case J.toObject json of
    Nothing -> Lit <$> Codec.decode valueCodec json
    Just o -> case stringAt o "config", stringAt o "fact" of
      Just k, _ -> Right (ConfigOf (ConfigId k))
      _, Just k -> Right (FactOf (FactId k))
      _, _ -> Left (CA.TypeMismatch "an expression: a literal, {config}, or {fact}")

guardCodec :: CA.JsonCodec Guard
guardCodec = CA.fix \self -> Codec.codec' (decode self) (encode self)
  where
  encode self = case _ of
    Holds e -> Codec.encode exprCodec e
    Not g -> obj [ Tuple "not" (Codec.encode self g) ]
    And gs -> obj [ Tuple "all" (J.fromArray (map (Codec.encode self) gs)) ]
    Or gs -> obj [ Tuple "any" (J.fromArray (map (Codec.encode self) gs)) ]
    Cmp op l r -> obj
      [ Tuple "op" (Codec.encode cmpOpCodec op)
      , Tuple "left" (Codec.encode exprCodec l)
      , Tuple "right" (Codec.encode exprCodec r)
      ]
  decode self json = case J.toObject json of
    Nothing -> Holds <$> Codec.decode exprCodec json
    Just o -> case Object.lookup "not" o, Object.lookup "all" o, Object.lookup "any" o, Object.lookup "op" o of
      Just inner, _, _, _ -> Not <$> Codec.decode self inner
      _, Just arr, _, _ -> And <$> Codec.decode (CA.array self) arr
      _, _, Just arr, _ -> Or <$> Codec.decode (CA.array self) arr
      _, _, _, Just opJson -> do
        op <- Codec.decode cmpOpCodec opJson
        l <- required o "left" >>= Codec.decode exprCodec
        r <- required o "right" >>= Codec.decode exprCodec
        pure (Cmp op l r)
      _, _, _, _ -> Holds <$> Codec.decode exprCodec json

-- =============================================================================
-- Rules
-- =============================================================================

-- | `{"to": "armed"}`, `{"stay": true}` or `{"refuse": "converter-busy"}`,
-- | optionally with a `when`. Exactly one outcome key must be present.
caseCodec :: CA.JsonCodec Case
caseCodec = Codec.codec' decode encode
  where
  encode c =
    obj (whenPart <> outcomePart)
    where
    whenPart = case c.when of
      Nothing -> []
      Just g -> [ Tuple "when" (Codec.encode guardCodec g) ]
    outcomePart = case c.outcome of
      Move sid -> [ Tuple "to" (Codec.encode stateIdCodec sid) ]
      Stay -> [ Tuple "stay" (J.fromBoolean true) ]
      Refuse rid -> [ Tuple "refuse" (Codec.encode refusalIdCodec rid) ]
  decode json = do
    o <- note (CA.TypeMismatch "a case object") (J.toObject json)
    when' <- case Object.lookup "when" o of
      Nothing -> Right Nothing
      Just g -> Just <$> Codec.decode guardCodec g
    outcome <- case stringAt o "to", Object.lookup "stay" o, stringAt o "refuse" of
      Just sid, _, _ -> Right (Move (StateId sid))
      _, Just _, _ -> Right Stay
      _, _, Just rid -> Right (Refuse (RefusalId rid))
      _, _, _ -> Left (CA.TypeMismatch "a case needs exactly one of to, stay or refuse")
    pure { when: when', outcome }

ruleCodec :: CA.JsonCodec Rule
ruleCodec = Codec.codec' decode encode
  where
  encode r = obj
    [ Tuple "from" (Codec.encode stateIdCodec r.from)
    , Tuple "on" (Codec.encode eventIdCodec r.on)
    , Tuple "cases" (J.fromArray (map (Codec.encode caseCodec) r.cases))
    ]
  decode json = do
    o <- note (CA.TypeMismatch "a rule object") (J.toObject json)
    from <- StateId <$> requiredString o "from"
    on <- EventId <$> requiredString o "on"
    cases <- required o "cases" >>= Codec.decode (CA.array caseCodec)
    pure { from, on, cases }

deadlineCodec :: CA.JsonCodec Deadline
deadlineCodec = Codec.codec' decode encode
  where
  encode d = obj
    [ Tuple "in" (Codec.encode stateIdCodec d.inState)
    , Tuple "fires" (Codec.encode eventIdCodec d.fires)
    , Tuple "after" (Codec.encode exprCodec d.after)
    ]
  decode json = do
    o <- note (CA.TypeMismatch "a deadline object") (J.toObject json)
    inState <- StateId <$> requiredString o "in"
    fires <- EventId <$> requiredString o "fires"
    after <- required o "after" >>= Codec.decode exprCodec
    pure { inState, fires, after }

-- =============================================================================
-- Declarations
-- =============================================================================

stateDeclCodec :: CA.JsonCodec StateDecl
stateDeclCodec = Codec.codec' decode encode
  where
  encode s = obj
    ( [ Tuple "id" (Codec.encode stateIdCodec s.id)
      , Tuple "label" (J.fromString s.label)
      ] <> if s.final then [ Tuple "final" (J.fromBoolean true) ] else []
    )
  decode json = do
    o <- note (CA.TypeMismatch "a state object") (J.toObject json)
    raw <- requiredString o "id"
    let label = fromMaybe raw (stringAt o "label")
    pure { id: StateId raw, label, final: boolAt o "final" }

eventDeclCodec :: CA.JsonCodec EventDecl
eventDeclCodec = Codec.codec' decode encode
  where
  encode e = obj
    [ Tuple "id" (Codec.encode eventIdCodec e.id)
    , Tuple "label" (J.fromString e.label)
    , Tuple "source" (Codec.encode eventSourceCodec e.source)
    ]
  decode json = do
    o <- note (CA.TypeMismatch "an event object") (J.toObject json)
    raw <- requiredString o "id"
    let label = fromMaybe raw (stringAt o "label")
    -- Absent means user-driven: the common case, and the one an author writing
    -- an artifact by hand should not have to say.
    source <- maybe (Right FromUser) (Codec.decode eventSourceCodec) (Object.lookup "source" o)
    pure { id: EventId raw, label, source }

configDeclCodec :: CA.JsonCodec ConfigDecl
configDeclCodec = Codec.codec' decode encode
  where
  encode c = obj
    [ Tuple "id" (Codec.encode configIdCodec c.id)
    , Tuple "label" (J.fromString c.label)
    , Tuple "default" (Codec.encode valueCodec c.default)
    ]
  decode json = do
    o <- note (CA.TypeMismatch "a config object") (J.toObject json)
    raw <- requiredString o "id"
    let label = fromMaybe raw (stringAt o "label")
    def <- required o "default" >>= Codec.decode valueCodec
    pure { id: ConfigId raw, label, default: def }

factDeclCodec :: CA.JsonCodec FactDecl
factDeclCodec = Codec.codec' decode encode
  where
  encode f = obj
    [ Tuple "id" (Codec.encode factIdCodec f.id)
    , Tuple "label" (J.fromString f.label)
    ]
  decode json = do
    o <- note (CA.TypeMismatch "a fact object") (J.toObject json)
    raw <- requiredString o "id"
    pure { id: FactId raw, label: fromMaybe raw (stringAt o "label") }

refusalDeclCodec :: CA.JsonCodec RefusalDecl
refusalDeclCodec = Codec.codec' decode encode
  where
  encode r = obj
    [ Tuple "id" (Codec.encode refusalIdCodec r.id)
    , Tuple "text" (J.fromString r.text)
    ]
  decode json = do
    o <- note (CA.TypeMismatch "a refusal object") (J.toObject json)
    raw <- requiredString o "id"
    pure { id: RefusalId raw, text: fromMaybe raw (stringAt o "text") }

-- =============================================================================
-- The whole artifact
-- =============================================================================

specCodec :: CA.JsonCodec Spec
specCodec = Codec.codec' decode encode
  where
  encode s = obj
    [ Tuple "glassbox" (Codec.encode CA.int s.version)
    , Tuple "id" (J.fromString s.id)
    , Tuple "title" (J.fromString s.title)
    , Tuple "initial" (Codec.encode stateIdCodec s.initial)
    , Tuple "states" (J.fromArray (map (Codec.encode stateDeclCodec) s.states))
    , Tuple "events" (J.fromArray (map (Codec.encode eventDeclCodec) s.events))
    , Tuple "config" (J.fromArray (map (Codec.encode configDeclCodec) s.config))
    , Tuple "facts" (J.fromArray (map (Codec.encode factDeclCodec) s.facts))
    , Tuple "refusals" (J.fromArray (map (Codec.encode refusalDeclCodec) s.refusals))
    , Tuple "rules" (J.fromArray (map (Codec.encode ruleCodec) s.rules))
    , Tuple "deadlines" (J.fromArray (map (Codec.encode deadlineCodec) s.deadlines))
    ]
  decode json = do
    o <- note (CA.TypeMismatch "a Glassbox artifact object") (J.toObject json)
    version <- lmap (CA.AtKey "glassbox") (required o "glassbox" >>= Codec.decode CA.int)
    -- Refuse a future format outright rather than decode the parts we recognise.
    -- Silently ignoring fields we do not understand is how one implementation
    -- comes to run a different machine from another, which is the failure this
    -- format exists to prevent.
    when (version > formatVersion)
      (Left (CA.AtKey "glassbox" (CA.TypeMismatch ("format version " <> show version <> "; this build reads " <> show formatVersion))))
    id <- requiredString o "id"
    initial <- StateId <$> requiredString o "initial"
    states <- listAt o "states" stateDeclCodec
    events <- listAt o "events" eventDeclCodec
    config <- listAt o "config" configDeclCodec
    facts <- listAt o "facts" factDeclCodec
    refusals <- listAt o "refusals" refusalDeclCodec
    rules <- listAt o "rules" ruleCodec
    deadlines <- listAt o "deadlines" deadlineCodec
    pure
      { version
      , id
      , title: fromMaybe id (stringAt o "title")
      , initial
      , states
      , events
      , config
      , facts
      , refusals
      , rules
      , deadlines
      }

-- =============================================================================
-- Helpers
-- =============================================================================

obj :: Array (Tuple String Json) -> Json
obj = J.fromObject <<< Object.fromFoldable

stringAt :: Object.Object Json -> String -> Maybe String
stringAt o k = Object.lookup k o >>= J.toString

boolAt :: Object.Object Json -> String -> Boolean
boolAt o k = fromMaybe false (Object.lookup k o >>= J.toBoolean)

required :: Object.Object Json -> String -> Either CA.JsonDecodeError Json
required o k = note (CA.AtKey k CA.MissingValue) (Object.lookup k o)

requiredString :: Object.Object Json -> String -> Either CA.JsonDecodeError String
requiredString o k = note (CA.AtKey k (CA.TypeMismatch "a string")) (stringAt o k)

-- | An absent list decodes as empty. A machine with no config, no facts or no
-- | deadlines is entirely ordinary, and requiring an empty array for each would
-- | make the smallest artifact noisy for no gain.
listAt :: forall a. Object.Object Json -> String -> CA.JsonCodec a -> Either CA.JsonDecodeError (Array a)
listAt o k codec = case Object.lookup k o of
  Nothing -> Right []
  Just arr -> lmap (CA.AtKey k) (Codec.decode (CA.array codec) arr)


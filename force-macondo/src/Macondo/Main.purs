-- | *Cien años de soledad* as a graph: who appears with whom.
-- |
-- | The page plays itself on arrival (`Macondo.Script`): the family tree as
-- | the novel brings each Buendía on stage, then the whole cast in open
-- | country, in colour, in the lands of their generation and of their
-- | generation and sex. Any button takes over; Replay starts again. A button
-- | changes the forces on the running simulation and nothing else, and new
-- | lands paint themselves while the people walk to them.
-- |
-- | Everything runs on a frame clock: requestAnimationFrame for a visitor,
-- | `window.macondo.step(n)` for the film (`?film`), so the film is this page
-- | frame for frame.
module Macondo.Main (main) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Int (toNumber)
import Data.Number (cos, sin)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Effect (Effect, forE)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Hylograph.HATS (Tree)
import Hylograph.HATS.InterpreterTick (clearContainer, rerender)
import Hylograph.HATS.Transitions (HATSTransitions, TransitionResult(..), tickTransitions)
import Hylograph.Simulation.Stepped (Engine(..), Point)
import Hylograph.Simulation.Stepped as Stepped
import Hylograph.Transition.RAF (requestAnimationFrame)
import Macondo.Chart (chart, controls, passage, readout)
import Macondo.Clock (exposeStep, filmMode, reload, setOpacity)
import Macondo.Physics (Body, Land, Mode(..), bodies, degreesOf, generationWidth, geography, inside, landsOf, physics, sameForces, stage, ties)
import Macondo.Script (Scene, cues, scene)
import Macondo.Script as Script
import Macondo.Story (Story)
import Macondo.Story as Story
import Macondo.Wash (wash)

main :: Effect Unit
main = launchAff_ do
  loaded <- Story.load "./data/cien-anos.json"
  case loaded of
    Left problem -> liftEffect (Console.error problem)
    Right story -> liftEffect (run story)

type State =
  { frame :: Int
  , playing :: Boolean -- the script is in charge
  , mode :: Mode
  , arrived :: Set Int -- on stage, under the forces
  , seen :: Set Int -- on stage or on their way there
  , ticks :: Int -- since the forces last changed
  , shown :: Maybe Scene -- the scene last drawn from the script
  }

run :: Story -> Effect Unit
run story = do
  -- Two tempi: the film is slower, laid against its soundtrack.
  film <- filmMode
  let pace = if film then Script.film else Script.web
  let atlas = geography story
  let degrees = degreesOf story
  let together = ties story
  let everyone = Set.fromFoldable (Array.range 0 (Array.length story.people - 1))
  let forcesOf = physics atlas degrees together.kinds
  simulation <- Stepped.create Default (forcesOf Set.empty Tree) (bodies atlas story) together.links
  state <- Ref.new { frame: 0, playing: true, mode: Tree, arrived: Set.empty, seen: Set.empty, ticks: 0, shown: Nothing }
  -- Newcomers in flight: where each set out from and where they are going.
  flights <- Ref.new (Map.empty :: Map Int { from :: Point, to :: Point })
  painting <- Ref.new ([] :: Array HATSTransitions)

  let
    draw people = do
      current <- Ref.read state
      alpha <- simulation.alpha
      void $ rerender "#graph" (chart atlas story current.mode current.seen current.arrived people)
      -- Not shown: read by the checks.
      void $ rerender "#readout" $ readout story
        { mode: current.mode, ticks: current.ticks, alpha, outside: outside (landsOf atlas current.mode) (landOf current.mode) people }

    -- Clear first: a rerender against the same keys is an update, which has
    -- no enter phase, so the layers would never paint themselves again.
    paint :: Tree -> Effect Unit
    paint tree = do
      clearContainer "#wash"
      result <- rerender "#wash" tree
      Ref.write (Array.fromFoldable result.transitions) painting

    -- Go to a mode, with these people on stage. Someone joining the tree
    -- warms it only a little: they have been carried to about their place,
    -- and the others need not move for them.
    goTo mode arrived = do
      current <- Ref.read state
      when (mode /= current.mode || arrived /= current.arrived) do
        let moved = not (sameForces mode current.mode) || arrived /= current.arrived
        let joining = mode == current.mode && arrived /= current.arrived
        let setup = forcesOf arrived mode
        -- A new mode starts a little cool, so that its first tick, with every
        -- force suddenly changed, is a step and not a leap.
        when moved $ simulation.applySetup
          if joining then setup { params = setup.params { alpha = 0.3 } } else setup { params = setup.params { alpha = 0.6 } }
        when (mode /= current.mode && not (sameForces mode current.mode)) (paint (wash atlas mode))
        Ref.modify_ (_ { mode = mode, arrived = arrived, ticks = if moved then 0 else current.ticks }) state
        void $ rerender "#controls" (controls choose replay mode)
        draw =<< simulation.nodes

    -- A button: the visitor takes over from the script.
    choose mode = do
      Ref.modify_ (_ { playing = false }) state
      -- The first sentence stays, whole; the last words go.
      let whole = scene pace story 100000
      void $ rerender "#opening" (passage whole.opening)
      void $ rerender "#closing" (passage [])
      setOpacity "#wash, #graph" 1.0
      flying <- Ref.read flights
      for_ (Map.keys flying) \index -> simulation.pin index Nothing
      Ref.write Map.empty flights
      Ref.modify_ (_ { seen = everyone }) state
      goTo mode everyone

    replay = reload

    -- Carry the newcomers: each sets out from where they wait, towards about
    -- their place in the tree, and is held on an eased path until they land.
    carry now = do
      people <- simulation.nodes
      flying <- Ref.read flights
      for_ now.flying \f -> do
        -- Where they set out from is fixed; where they are going follows the
        -- family as it moves, so that they land where the rules want them now
        -- and not where they wanted them when the flight began.
        let to = if now.mode == Tree then landing story now.arrived people f.index else among story now.arrived people f.index
        route <- case Map.lookup f.index flying of
          Just route -> pure (route { to = to })
          Nothing -> do
            let from = maybe { x: 0.0, y: 0.0 } (\b -> { x: b.x, y: b.y }) (Array.index people f.index)
            pure { from, to }
        Ref.modify_ (Map.insert f.index route) flights
        let t = f.progress * f.progress * (3.0 - 2.0 * f.progress)
        simulation.pin f.index (Just { x: route.from.x + t * (route.to.x - route.from.x), y: route.from.y + t * (route.to.y - route.from.y) })
      -- Whoever has landed is let go, to the forces.
      for_ (Map.keys flying) \index -> when (Set.member index now.arrived) do
        simulation.pin index Nothing
        Ref.modify_ (Map.delete index) flights
      Ref.modify_ (_ { seen = now.arrived <> Set.fromFoldable (_.index <$> now.flying) }) state

    -- The script's scene for this frame, if the script is in charge.
    direct = do
      current <- Ref.read state
      when current.playing do
        let now = scene pace story current.frame
        carry now
        goTo now.mode now.arrived
        when (map _.opening current.shown /= Just now.opening) do
          void $ rerender "#opening" (passage now.opening)
        when (map _.closing current.shown /= Just now.closing) do
          void $ rerender "#closing" (passage now.closing)
        when (map _.presence current.shown /= Just now.presence) do
          setOpacity "#wash, #graph" now.presence
        Ref.modify_ (_ { shown = Just now, playing = not now.done }) state

    -- The paint is stepped a fixed 16 ms a frame, as onion's recipes do.
    brush = do
      running <- Ref.read painting
      unless (Array.null running) do
        stepped <- running # traverse \transitions -> tickTransitions 16.0 transitions <#> case _ of
          Complete -> Nothing
          Running next -> Just next
        Ref.write (Array.catMaybes stepped) painting

    -- One frame: the script, the paint, one tick of the simulation.
    frame = do
      direct
      brush
      alpha <- simulation.alpha
      carrying <- not <<< Map.isEmpty <$> Ref.read flights
      when (alpha > 0.001 || carrying) do
        people <- simulation.step
        Ref.modify_ (\current -> current { ticks = current.ticks + 1 }) state
        draw people
      Ref.modify_ (\current -> current { frame = current.frame + 1 }) state

    loop _ = do
      frame
      void $ requestAnimationFrame loop

  void $ rerender "#controls" (controls choose replay Tree)
  draw =<< simulation.nodes
  if film then exposeStep (\n -> forE 0 n \_ -> frame) (cues pace story)
  else void $ requestAnimationFrame loop

-- | About where a newcomer belongs in the tree: a generation to the right of
-- | their parents on stage, at their height; else just below a spouse on
-- | stage; else in their generation's column, at the middle. Then moved to
-- | satisfy every rule the tree holds them to (`Macondo.Physics`), against
-- | everyone already on stage: right of parents, left of children, beside a
-- | spouse. A landing that broke one would be snapped into line by the hard
-- | limit in a single frame, which is the jump the flight is there to avoid.
landing :: Story -> Set Int -> Array Body -> Int -> Point
landing story arrived people index =
  case Array.index story.people index of
    Nothing -> { x: stage.width / 2.0, y: stage.height / 2.0 }
    Just person ->
      let
        here = Array.filter (\b -> Set.member b.id arrived) people
        parents = Array.filter (\b -> Array.elem b.person.id person.parents) here
        children = Array.filter (\b -> Array.elem person.id b.person.parents) here
        -- couples run from the one born into the family to the one who
        -- married in (`Macondo.Physics.ties`)
        bornIn id = maybe false (not <<< Array.null <<< _.parents) (Array.find (\p -> p.id == id) story.people)
        -- The couple's tie runs from this end, as `ties` orients it.
        sourceOf c = if bornIn c.b && not (bornIn c.a) then c.b else c.a
        partners = here # Array.mapMaybe \b -> do
          couple <- Array.find (\c -> (c.a == person.id && c.b == b.person.id) || (c.b == person.id && c.a == b.person.id)) story.couples
          -- whether this newcomer is the one the tie runs to: they stand at
          -- or right of the partner, a little below
          pure { body: b, below: sourceOf couple == b.person.id }
        base = case Array.uncons parents, Array.head partners of
          Just _, _ ->
            { x: foldl (\m b -> max m b.x) (-1.0e9) parents + generationWidth
            , y: foldl (\m b -> m + b.y) 0.0 parents / toNumber (Array.length parents)
            }
          Nothing, Just p -> { x: p.body.x, y: p.body.y + (if p.below then 24.0 else -24.0) }
          Nothing, Nothing -> { x: stage.width / 2.0 + generationWidth * (toNumber person.era - 4.0), y: stage.height / 2.0 }
        rightOfParents at = at { x = foldl (\m b -> max m (b.x + generationWidth)) at.x parents }
        leftOfChildren at = at { x = foldl (\m b -> min m (b.x - generationWidth)) at.x children }
        besideSpouses at = foldl
          ( \a p ->
              if p.below then a { x = max a.x p.body.x, y = max a.y (p.body.y + 24.0) }
              else a { x = min a.x p.body.x, y = min a.y (p.body.y - 24.0) }
          )
          at
          partners
      in
        leftOfChildren (besideSpouses (rightOfParents base))

-- | Where someone outside the family lands in open country: among the people
-- | they share passages with who are already there (their mean position, a
-- | little apart from it so no two land on one point), or at the middle.
among :: Story -> Set Int -> Array Body -> Int -> Point
among story arrived people index =
  case Array.index story.people index of
    Nothing -> middle
    Just person ->
      let
        met = story.meetings # Array.mapMaybe \m ->
          if m.source == person.id then Just m.target else if m.target == person.id then Just m.source else Nothing
        near = Array.filter (\b -> Set.member b.id arrived && Array.elem b.person.id met) people
        n = toNumber (Array.length near)
        angle = toNumber index * 2.399963
      in
        if Array.null near then middle
        else
          { x: foldl (\m b -> m + b.x) 0.0 near / n + 18.0 * cos angle
          , y: foldl (\m b -> m + b.y) 0.0 near / n + 18.0 * sin angle
          }
  where
  middle = { x: stage.width / 2.0, y: stage.height / 2.0 }

-- | Which land a body is in, in a mode.
landOf :: Mode -> Body -> Maybe Int
landOf = case _ of
  Generations -> _.generation
  GenerationsBySex -> _.cell
  _ -> const Nothing

-- | How many people's discs are not wholly inside their land (none, in open
-- | country, where there are no lands).
outside :: Array Land -> (Body -> Maybe Int) -> Array Body -> Int
outside lands landOfBody = Array.length <<< Array.filter out
  where
  out body = case landOfBody body >>= Array.index lands of
    Nothing -> false
    Just land -> not (inside land.shape { x: body.x, y: body.y, radius: body.radius })

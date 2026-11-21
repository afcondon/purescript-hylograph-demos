-- | The script: what the page shows at each frame when it plays by itself.
-- |
-- | A pure function of the pace and the frame number, so that a visitor's
-- | page and the film, stepped a frame at a time, show the same thing at the
-- | same frame. Sixty frames to the second.
-- |
-- | The novel's first sentence fades in across the top, a word at a time,
-- | with a breath at each comma and a beat before the colonel's name, which
-- | has a line to itself; the colonel arrives on his name and his father on
-- | *su padre*. The rest of the family follows in the order the novel first
-- | brings them on stage, each carried to its place in the tree. Then the whole
-- | cast in open country, in colour, in the lands of their generation and of
-- | their generation and sex; and the novel's last words fade in across the
-- | foot as the picture goes, as Macondo does, with the wind.
-- |
-- | Two tempi: the web page is brisk, since it is watched in a browser; the
-- | film is slower, laid against its soundtrack (`film/render.mjs`).
-- |
-- | Both passages are García Márquez's own, quoted to frame a work about the
-- | novel, and cited at the foot of the page.
module Macondo.Script
  ( Scene
  , Line(..)
  , Word
  , Pace
  , Cues
  , web
  , film
  , scene
  , cues
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Macondo.Physics (Mode(..))
import Macondo.Story (Story)

-- | Which line of the page a word sits on.
data Line
  = Banner -- the first words, large
  | Name -- the colonel's name, as large, a line to itself
  | Remembering -- the rest of the first sentence, smaller
  | Ending -- the last words, at the foot

derive instance Eq Line

-- | A word and how far it has faded in, from 0 to 1.
type Word = { text :: String, line :: Line, shown :: Number }

type Scene =
  { mode :: Mode
  , arrived :: Set Int -- people, by index, on stage
  , flying :: Array { index :: Int, progress :: Number } -- on their way there
  , opening :: Array Word
  , closing :: Array Word
  , presence :: Number -- how much of the picture is left: 1, until the end
  , done :: Boolean
  }

-- | How long everything takes, in frames.
type Pace =
  { openingStart :: Int -- before the first word
  , fadeIn :: Int -- a word comes up over this long
  , perWord :: Int -- and the next starts this much later
  , afterComma :: Int -- a breath after a comma
  , beforeLine :: Int -- a beat at a new line
  , beforeFamily :: Int -- from the end of the sentence to the first arrival
  , perArrival :: Int -- between one Buendía and the next
  , flight :: Int -- the carrying of each newcomer
  , treeHold :: Int -- the finished tree, before open country
  , perOther :: Int -- between one of the rest of the cast and the next
  , openCountry :: Int
  , inColour :: Int
  , generations :: Int
  , bySex :: Int -- before the last words begin
  , endHold :: Int -- after them
  }

-- | The web page: brisk.
web :: Pace
web =
  { openingStart: 30, fadeIn: 24, perWord: 6, afterComma: 14, beforeLine: 32
  , beforeFamily: 60, perArrival: 22, flight: 40, treeHold: 120, perOther: 4
  , openCountry: 300, inColour: 120, generations: 300, bySex: 330, endHold: 180
  }

-- | The film: about a minute, laid against its soundtrack. The command
-- | "¡armas al hombro!" of "Batalla de Peralonso" falls a beat after
-- | *fusilamiento*, and its "¡Colombia!" on the frame the generations begin
-- | to be painted (`film/render.mjs` places the sound by `cues`, and checks
-- | the second).
film :: Pace
film =
  { openingStart: 90, fadeIn: 36, perWord: 9, afterComma: 22, beforeLine: 50
  , beforeFamily: 80, perArrival: 26, flight: 50, treeHold: 121, perOther: 6
  , openCountry: 330, inColour: 190, generations: 600, bySex: 540, endHold: 240
  }

type Text = { text :: String, line :: Line }

wordsOf :: Line -> String -> Array Text
wordsOf line = map { text: _, line } <<< split (Pattern " ")

opening :: Array Text
opening =
  wordsOf Banner "Muchos años después, frente al pelotón de fusilamiento,"
    <> wordsOf Name "el coronel Aureliano Buendía"
    <> wordsOf Remembering "había de recordar aquella tarde remota en que su padre lo llevó a conocer el hielo."

closing :: Array Text
closing = wordsOf Ending
  "…y que todo lo escrito en ellos era irrepetible desde siempre y para siempre, porque las estirpes condenadas a cien años de soledad no tenían una segunda oportunidad sobre la tierra."

-- | When each word starts to fade in, beginning at `start`: several are
-- | coming up at once, so the line flows rather than ticks.
cue :: Pace -> Int -> Array Text -> Array Int
cue pace start texts = (foldl step { at: start, previous: Nothing, out: [] } texts).out
  where
  step acc w =
    let
      pause = case acc.previous of
        Nothing -> 0
        Just p
          | p.line /= w.line -> pace.beforeLine
          | endsWith "," p.text -> pace.afterComma
          | otherwise -> 0
      at = acc.at + pause
    in
      { at: at + pace.perWord, previous: Just w, out: Array.snoc acc.out at }
  endsWith suffix text = CodeUnits.takeRight (CodeUnits.length suffix) text == suffix

-- | How far a word that starts at `at` has faded in by `frame`: eased, so it
-- | arrives softly and settles softly.
shownAt :: Pace -> Int -> Int -> Number
shownAt pace at frame =
  let
    t = max 0.0 (min 1.0 (toNumber (frame - at) / toNumber pace.fadeIn))
  in
    t * t * (3.0 - 2.0 * t)

-- | Everything that happens, at the frame it happens.
type Timeline =
  { openingCues :: Array Int
  , arrivals :: Array { at :: Int, index :: Int }
  , treeEnd :: Int
  , inColour :: Int
  , generations :: Int
  , bySex :: Int
  , closingCues :: Array Int
  , closingStart :: Int
  , closingEnd :: Int
  , end :: Int
  }

timeline :: Pace -> Story -> Timeline
timeline pace story =
  { openingCues
  , arrivals: [ colonel, father ] <> stream <> world
  , treeEnd
  , inColour
  , generations
  , bySex
  , closingCues
  , closingStart
  , closingEnd
  , end: closingEnd + pace.endHold
  }
  where
  openingCues = cue pace pace.openingStart opening
  openingEnd = fromMaybe pace.openingStart (Array.last openingCues) + pace.fadeIn
  cueOf text = fromMaybe pace.openingStart do
    index <- Array.findIndex (\w -> w.text == text) opening
    Array.index openingCues index
  indexOf id = fromMaybe 0 (Array.findIndex (\person -> person.id == id) story.people)
  -- The colonel sets out as his name comes up; his father as "padre" does.
  colonel = { at: cueOf "Buendía", index: indexOf "colonel-aureliano" }
  father = { at: cueOf "padre", index: indexOf "jose-arcadio-buendia" }
  streamStart = openingEnd + pace.beforeFamily
  family = story.people
    # Array.mapWithIndex (\index person -> { index, person })
    # Array.filter (\each -> each.person.family && each.index /= colonel.index && each.index /= father.index)
    # Array.sortWith (\each -> Tuple each.person.first each.index)
  stream = family # Array.mapWithIndex \order each -> { at: streamStart + pace.perArrival * order, index: each.index }
  treeEnd = streamStart + pace.perArrival * Array.length family + pace.flight + pace.treeHold
  -- At open country everyone else comes in, a few frames apart, in the
  -- order the novel brings them on.
  others = story.people
    # Array.mapWithIndex (\index person -> { index, person })
    # Array.filter (not <<< _.person.family)
    # Array.sortWith (\each -> Tuple each.person.first each.index)
  world = others # Array.mapWithIndex \order each -> { at: treeEnd + pace.perOther * order, index: each.index }
  inColour = treeEnd + pace.openCountry
  generations = inColour + pace.inColour
  bySex = generations + pace.generations
  closingStart = bySex + pace.bySex
  closingCues = cue pace closingStart closing
  closingEnd = fromMaybe closingStart (Array.last closingCues) + pace.fadeIn

-- | The frames the soundtrack is laid against.
type Cues =
  { fusilamiento :: Int -- the word starts to come up
  , openCountry :: Int
  , generations :: Int
  , closingStart :: Int
  , closingEnd :: Int
  , end :: Int
  }

cues :: Pace -> Story -> Cues
cues pace story =
  { fusilamiento: fromMaybe 0 do
      index <- Array.findIndex (\w -> w.text == "fusilamiento,") opening
      Array.index t.openingCues index
  , openCountry: t.treeEnd
  , generations: t.generations
  , closingStart: t.closingStart
  , closingEnd: t.closingEnd
  , end: t.end
  }
  where
  t = timeline pace story

scene :: Pace -> Story -> Int -> Scene
scene pace story frame =
  { mode
  , arrived: Set.fromFoldable (_.index <$> Array.filter (\a -> a.at + pace.flight <= frame) t.arrivals)
  , flying: t.arrivals # Array.mapMaybe \a ->
      if a.at <= frame && frame < a.at + pace.flight then Just { index: a.index, progress: toNumber (frame - a.at) / toNumber pace.flight }
      else Nothing
  , opening: Array.zipWith (\w at -> { text: w.text, line: w.line, shown: shownAt pace at frame }) opening t.openingCues
  , closing: Array.zipWith (\w at -> { text: w.text, line: w.line, shown: shownAt pace at frame }) closing t.closingCues
  , presence
  , done: frame >= t.end
  }
  where
  t = timeline pace story
  mode
    | frame >= t.bySex = GenerationsBySex
    | frame >= t.generations = Generations
    | frame >= t.inColour = InColour
    | frame >= t.treeEnd = OpenCountry
    | otherwise = Tree
  -- As the last words come up, the picture fades towards the paper.
  presence
    | frame < t.closingStart = 1.0
    | otherwise =
        let
          gone = min 1.0 (toNumber (frame - t.closingStart) / toNumber (t.closingEnd - t.closingStart))
        in
          1.0 - 0.75 * gone * gone

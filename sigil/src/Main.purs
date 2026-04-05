-- | Signature Demo Page
-- |
-- | Renders every test signature in four tabs:
-- |   1. Readme (library description)
-- |   2. Sigils (full HTML semantic signatures)
-- |   3. Siglets (compact dot notation)
-- |   4. Signets (dots with rotated identifier labels)
module Demo.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

import Sigil.Html as Sigil
import Sigil.Parse (parseToRenderType, elideAST)
import Sigil.Examples (signatures, dataDecls, classDecls, typeSynonyms, foreignImports, TestDataDecl, TestClassDecl, TestTypeSynonym, TestForeignImport)

foreign import createRow :: String -> Int -> String -> String -> String -> String -> Boolean -> Effect Unit
foreign import createDeclRow :: String -> Int -> String -> String -> Effect Unit

main :: Effect Unit
main = do
  log "[SigilDemo] Rendering signature gallery"
  -- Pass 1: Sigils
  Array.foldM (\_ s -> renderSigil s) unit signatures
  -- Pass 2: Siglets
  Array.foldM (\_ s -> renderSiglet s) unit signatures
  -- Pass 3: Signets
  Array.foldM (\_ s -> renderSignet s) unit signatures
  -- Pass 4: Data type declarations
  Array.foldM (\_ d -> renderDataDecl d) unit dataDecls
  -- Pass 5: Class declarations
  Array.foldM (\_ c -> renderClassDecl c) unit classDecls
  -- Pass 6: Type synonyms
  Array.foldM (\_ t -> renderTypeSyn t) unit typeSynonyms
  -- Pass 7: Foreign imports
  Array.foldM (\_ f -> renderForeignImp f) unit foreignImports
  log "[SigilDemo] Done"

renderSigil :: { idx :: Int, category :: String, name :: String, sig :: String } -> Effect Unit
renderSigil s = do
  let containerId = "sigil-" <> show s.idx
  case parseToRenderType s.sig of
    Just ast -> do
      createRow "sigil-table" s.idx s.name s.category s.sig containerId true
      Sigil.renderSignatureInto ("#" <> containerId)
        { name: s.name, ast, typeParams: [], className: Nothing }
    Nothing -> do
      createRow "sigil-table" s.idx s.name s.category s.sig containerId false
      log $ "[SigilDemo] Parse failed: #" <> show s.idx <> " " <> s.name

renderSiglet :: { idx :: Int, category :: String, name :: String, sig :: String } -> Effect Unit
renderSiglet s = do
  let containerId = "siglet-" <> show s.idx
  case parseToRenderType s.sig of
    Just ast -> do
      createRow "siglet-table" s.idx s.name s.category s.sig containerId true
      Sigil.renderSigletInto ("#" <> containerId)
        { ast: elideAST ast }
    Nothing -> do
      createRow "siglet-table" s.idx s.name s.category s.sig containerId false
      log $ "[SigilDemo] Parse failed: #" <> show s.idx <> " " <> s.name

renderSignet :: { idx :: Int, category :: String, name :: String, sig :: String } -> Effect Unit
renderSignet s = do
  let containerId = "signet-" <> show s.idx
  case parseToRenderType s.sig of
    Just ast -> do
      createRow "signet-table" s.idx s.name s.category s.sig containerId true
      Sigil.renderSignetInto ("#" <> containerId)
        { ast: elideAST ast }
    Nothing -> do
      createRow "signet-table" s.idx s.name s.category s.sig containerId false
      log $ "[SigilDemo] Parse failed: #" <> show s.idx <> " " <> s.name

renderDataDecl :: TestDataDecl -> Effect Unit
renderDataDecl d = do
  let containerId = "datatype-" <> show d.idx
  createDeclRow "datatype-table" d.idx d.name containerId
  let ctors = map (\c ->
        { name: c.name
        , args: Array.concatMap (\s -> case parseToRenderType s of
            Just rt -> [rt]
            Nothing -> []
          ) c.argSigs
        }
      ) d.constructors
  Sigil.renderDataDeclInto ("#" <> containerId)
    { name: d.name, typeParams: d.typeParams, constructors: ctors, keyword: d.keyword }

renderClassDecl :: TestClassDecl -> Effect Unit
renderClassDecl c = do
  let containerId = "class-" <> show c.idx
  createDeclRow "class-table" c.idx c.name containerId
  let supers = map (\sc ->
        { name: sc.name
        , methods: map (\m ->
            { name: m.name, ast: m.sig >>= parseToRenderType }
          ) sc.methods
        }
      ) c.superclasses
  let methods = map (\m ->
        { name: m.name, ast: m.sig >>= parseToRenderType }
      ) c.methods
  Sigil.renderClassDeclInto ("#" <> containerId)
    { name: c.name, typeParams: c.typeParams, superclasses: supers, methods }

renderTypeSyn :: TestTypeSynonym -> Effect Unit
renderTypeSyn t = do
  let containerId = "typesyn-" <> show t.idx
  createDeclRow "typesyn-table" t.idx t.name containerId
  case parseToRenderType t.body of
    Just body ->
      Sigil.renderTypeSynonymInto ("#" <> containerId)
        { name: t.name, typeParams: t.typeParams, body }
    Nothing ->
      log $ "[SigilDemo] Type synonym parse failed: " <> t.name

renderForeignImp :: TestForeignImport -> Effect Unit
renderForeignImp f = do
  let containerId = "foreign-" <> show f.idx
  createDeclRow "foreign-table" f.idx f.name containerId
  case parseToRenderType f.sig of
    Just ast ->
      Sigil.renderForeignImportInto ("#" <> containerId)
        { name: f.name, ast }
    Nothing ->
      log $ "[SigilDemo] Foreign import parse failed: " <> f.name

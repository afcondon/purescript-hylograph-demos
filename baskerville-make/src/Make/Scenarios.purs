-- | The two embedded Makefiles.
-- |
-- | EDITOR WARNING: the string literals below contain REAL TAB
-- | characters — recipe lines must start with a tab or the parser
-- | (correctly) refuses them. Do not let an editor detab this file.
-- | The Ecosystem literal is a byte-exact copy of afc-work/Makefile,
-- | spliced in by script; regenerate rather than hand-edit.
module Make.Scenarios
  ( Scenario(..)
  , Loaded
  , loadScenario
  , scenarioSource
  , scenarioLabel
  , scenarioCaption
  ) where

import Prelude

import Data.Either (Either)
import Make.Model (BuildModel, Snapshot, allFresh, fromMakefile, sourcesOnly)
import Make.Parser (parseMakefileText)
import Parsing (ParseError)

data Scenario = Ecosystem | BuildChain

derive instance Eq Scenario

type Loaded = { model :: BuildModel, snapshot :: Snapshot }

loadScenario :: Scenario -> Either ParseError Loaded
loadScenario sc = parseMakefileText (scenarioSource sc) <#> \mf ->
  let
    model = fromMakefile mf
  in
    { model, snapshot: initialSnapshot sc model }

-- | BuildChain starts just-built (everything fresh — the first touch
-- | tells the whole story); Ecosystem starts as a fresh checkout
-- | (sources exist, targets don't).
initialSnapshot :: Scenario -> BuildModel -> Snapshot
initialSnapshot = case _ of
  Ecosystem -> sourcesOnly
  BuildChain -> allFresh

scenarioLabel :: Scenario -> String
scenarioLabel = case _ of
  Ecosystem -> "afc-work ecosystem"
  BuildChain -> "build chain"

scenarioCaption :: Scenario -> String
scenarioCaption = case _ of
  Ecosystem ->
    "The real top-level Makefile of the afc-work ecosystem — almost every \
    \target is .PHONY: make as command-runner, not build system. The five \
    \red targets are rules never declared phony, so make sees them as \
    \missing files and rebuilds them every run. Staleness lives in the \
    \other scenario."
  BuildChain ->
    "A curated build chain: sources feed compiled output, bundles, and a \
    \site, with shared/style.css braiding into two targets. Touch a source \
    \and watch the staleness cascade."

scenarioSource :: Scenario -> String
scenarioSource = case _ of
  Ecosystem -> ecosystemMakefile
  BuildChain -> buildChainMakefile

buildChainMakefile :: String
buildChainMakefile =
  """
# A small, honest build chain: sources -> compiled output -> bundles -> site.
# shared/style.css feeds two targets: the braid in the Sankey.

SRCS := src/Main.purs src/App.purs src/Rules.purs
STYLE := shared/style.css

output/index.js: $(SRCS) spago.yaml
	spago build

public/bundle.js: output/index.js
	esbuild output/index.js --bundle --outfile=public/bundle.js

public/index.html: site/template.html $(STYLE)
	render-template site/template.html > public/index.html

docs/demo/bundle.js: public/bundle.js
	cp public/bundle.js docs/demo/

docs/demo/index.html: public/index.html $(STYLE)
	cp public/index.html docs/demo/

.PHONY: site
site: docs/demo/bundle.js docs/demo/index.html
	@echo site assembled
"""

ecosystemMakefile :: String
ecosystemMakefile =
  """
# Hylograph Ecosystem - Top Level Makefile
# =========================================
#
# Orchestrates builds across multiple repos:
#   - purescript-polyglot (website + blog)
#   - purescript-hylograph-showcases (demo apps)
#   - CodeExplorer (minard, type-explorer)
#   - purescript-hylograph-libs (published libraries)
#
# Usage:
#   make help           - Show all targets
#   make polyglot       - Build website and blog
#   make docker-up      - Start all docker services
#   make docker-down    - Stop all docker services

SHELL := /bin/bash
.ONESHELL:

# ============================================================================
# DIRECTORIES
# ============================================================================

POLYGLOT := purescript-polyglot
SHOWCASES := purescript-hylograph-showcases
APPS := CodeExplorer
LIBS := purescript-hylograph-libs

# ============================================================================
# PHONY TARGETS
# ============================================================================

.PHONY: all help polyglot website blog lib-sites clean
.PHONY: docker-up docker-down docker-build docker-logs
.PHONY: docker-core docker-minard docker-hypo docker-tidal
.PHONY: check-tools

# ============================================================================
# TOP-LEVEL TARGETS
# ============================================================================

all: polyglot
	@echo "============================================"
	@echo "Build complete!"
	@echo "============================================"

# ============================================================================
# POLYGLOT (Website + Blog)
# ============================================================================

polyglot:
	@echo "Building polyglot (website + blog)..."
	$(MAKE) -C $(POLYGLOT) all

website:
	@echo "Building website..."
	$(MAKE) -C $(POLYGLOT) website

blog:
	@echo "Building blog..."
	$(MAKE) -C $(POLYGLOT) blog

lib-sites:
	@echo "Building library documentation sites..."
	$(MAKE) -C $(POLYGLOT) lib-sites

serve-website:
	$(MAKE) -C $(POLYGLOT) serve-website

serve-blog:
	$(MAKE) -C $(POLYGLOT) serve-blog

# ============================================================================
# DOCKER COMPOSE
# ============================================================================

docker-up:
	@echo "Starting all services (full profile)..."
	docker compose --profile full up -d

docker-down:
	@echo "Stopping all services..."
	docker compose down --remove-orphans

docker-build:
	@echo "Building all docker images..."
	docker compose --profile full build

docker-logs:
	docker compose logs -f

# Profile-specific targets
docker-core:
	@echo "Starting core services (edge + website)..."
	docker compose --profile core up -d

docker-minard:
	@echo "Starting minard profile..."
	docker compose --profile minard up -d

docker-hypo:
	@echo "Starting hypo-punter profile..."
	docker compose --profile hypo up -d

docker-tidal:
	@echo "Starting tidal profile..."
	docker compose --profile tidal up -d

docker-showcases:
	@echo "Starting showcases profile..."
	docker compose --profile showcases up -d

docker-libs:
	@echo "Starting library sites profile..."
	docker compose --profile libs up -d

# ============================================================================
# UTILITY TARGETS
# ============================================================================

clean:
	@echo "Cleaning all repos..."
	$(MAKE) -C $(POLYGLOT) clean || true
	@echo "Clean complete"

api-index:
	@node tools-for-agents/api-index/generate.mjs

check-tools:
	@echo "Checking build prerequisites..."
	@command -v spago >/dev/null 2>&1 && echo "  spago: OK" || echo "  spago: MISSING"
	@command -v purs >/dev/null 2>&1 && echo "  purs: OK" || echo "  purs: MISSING"
	@command -v node >/dev/null 2>&1 && echo "  node: OK" || echo "  node: MISSING"
	@command -v docker >/dev/null 2>&1 && echo "  docker: OK" || echo "  docker: MISSING"
	@echo ""

# ============================================================================
# HELP
# ============================================================================

help:
	@echo "Hylograph Ecosystem - Top Level Makefile"
	@echo "========================================="
	@echo ""
	@echo "Build targets:"
	@echo "  make all            - Build polyglot (website + blog)"
	@echo "  make polyglot       - Build website and blog"
	@echo "  make website        - Build just the website"
	@echo "  make blog           - Build just the blog"
	@echo "  make lib-sites      - Build library documentation sites"
	@echo "  make api-index      - Regenerate API-INDEX.md (greppable decl index)"
	@echo "  make clean          - Clean all build artifacts"
	@echo ""
	@echo "Serve targets (local development):"
	@echo "  make serve-website  - Serve website on :8080"
	@echo "  make serve-blog     - Serve blog on :8081"
	@echo ""
	@echo "Docker targets:"
	@echo "  make docker-up      - Start all services (full profile)"
	@echo "  make docker-down    - Stop all services"
	@echo "  make docker-build   - Build all docker images"
	@echo "  make docker-logs    - Follow docker logs"
	@echo ""
	@echo "Docker profiles:"
	@echo "  make docker-core      - Edge + website only"
	@echo "  make docker-minard    - Code cartography apps"
	@echo "  make docker-hypo      - Embedding/Grid explorers"
	@echo "  make docker-tidal     - Tidal music editor"
	@echo "  make docker-showcases - Other showcase apps"
	@echo "  make docker-libs      - Library documentation sites"
	@echo ""
	@echo "Utility:"
	@echo "  make check-tools    - Verify prerequisites"
	@echo ""
	@echo "Repository structure:"
	@echo "  $(POLYGLOT)/     - Website + blog"
	@echo "  $(SHOWCASES)/    - Showcase applications"
	@echo "  $(APPS)/         - CodeExplorer apps (minard, type-explorer)"
	@echo "  $(LIBS)/         - Published hylograph-* libraries"
	@echo ""
"""

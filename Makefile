##############################################################################
########################### Configuration options ############################
##############################################################################

# Working directory
WD = $(shell pwd)

# Elm sources and its entry point, Main.elm
SRCS = $(wildcard src/*.elm)
MAIN = src/Main.elm

# Custom HTML
SRC_INDEX = custom_index.html

# Assets relative directories
ASSETS_DIR = assets/
JS_DIR = $(ASSETS_DIR)js/
FA_DIR = $(ASSETS_DIR)fa/

# HTML and JS relative paths
INDEX = index.html
JS = $(JS_DIR)elm.js

# Output directories
DIST_DIR = $(WD)/dist/
DEBUG_DIR = $(DIST_DIR)debug/
PROD_DIR = $(DIST_DIR)prod/

# Debug outputs
DEBUG_JS = $(DEBUG_DIR)$(JS)
DEBUG_INDEX = $(DEBUG_DIR)$(INDEX)

# Prod outputs
PROD_JS = $(PROD_DIR)$(JS)
PROD_INDEX = $(PROD_DIR)$(INDEX)

# Doc options: directory, index file, white-list of files to generate their
# documentation (leave it empty if you want all files in SRCS) and a license
# name for the elm-doc --fake-license option
DOC_DIR = doc/
DOC_INDEX = $(DOC_DIR)index.html
DOC_FILES = src/Route.elm
DOC_FAKE_LICENSE = 'AGPL-3.0'

# Elm binary, elm make options and elm-live options
ELM_BIN = ~/.node_modules/bin/elm
ELM_MAKE_OPTS = --debug --output=$(DEBUG_JS)
ELM_LIVE_OPTS = --open --pushstate --dir=$(DEBUG_DIR)

# Remote machine SSH credentials and remote directory where the web is served
REMOTE = alejandro@rpi
REMOTE_DIR = /home/alejandro/web

##############################################################################
######################### Generation of actual files #########################
##############################################################################

$(DEBUG_DIR):
	mkdir -p $(DEBUG_DIR)$(JS_DIR)
	ln -s $(WD)/assets/fa/ $(DEBUG_DIR)$(ASSETS_DIR)

$(PROD_DIR):
	mkdir -p $(PROD_DIR)$(JS_DIR)
	ln -s $(WD)/assets/fa/ $(PROD_DIR)$(ASSETS_DIR)

# Generate a debug index file
$(DEBUG_INDEX) : $(DEBUG_DIR) $(SRC_INDEX)
	cp $(SRC_INDEX) $(DEBUG_INDEX)

# Compile a debug elm.js file from all the sources
$(DEBUG_JS) : $(DEBUG_DIR) $(SRCS)
	elm make $(MAIN) $(ELM_MAKE_OPTS)


$(PROD_INDEX): $(PROD_DIR) $(SRC_INDEX)
	cp $(SRC_INDEX) $(PROD_INDEX)

# Compile a production-ready, optimizied and minified elm.js
$(PROD_JS) : $(PROD_DIR) $(SRCS)
	./optimize.sh $(MAIN)

# Generate the documentation from all the sources
$(DOC_INDEX) : $(SRCS)
	elm-doc . --output $(DOC_DIR) \
	          --fake-license $(DOC_FAKE_LICENSE) \
	          --elm-path $(ELM_BIN) \
	          $(DOC_FILES)

##############################################################################
############################### PHONY targets ################################
##############################################################################

.PHONY: live debug prod deploy doc clean dist
.DEFAULT_GOAL := live

# Use elm-live to compile all the sources, watch their changes and recompile
# when a change is done, launching a server listening in localhost:8000
live : $(SRCS) $(DEBUG_JS) $(DEBUG_INDEX)
	elm-live $(MAIN) $(ELM_LIVE_OPTS) -- $(ELM_MAKE_OPTS)

# Compile all the sources into a debug elm.js file
debug : $(DEBUG_JS) $(DEBUG_INDEX)

# Compile all the sources into a production-ready, minified elm.js file
prod : $(PROD_JS) $(PROD_INDEX)

# Deploy the production-compiled project into the remote directory
deploy : $(PROD_JS) $(PROD_INDEX)
	ssh $(REMOTE) "cp -r $(REMOTE_DIR) $(REMOTE_DIR).backup"
	scp -r $(PROD_DIR)/* $(REMOTE):$(REMOTE_DIR)

# Generate all the documentation
doc : $(DOC_INDEX)

# Remove the whole dist/ directory
clean:
	rm -rf $(DIST_DIR)

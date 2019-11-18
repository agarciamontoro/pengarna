##############################################################################
########################### Configuration options ############################
##############################################################################

# Compiled .js files (debug and optimized)
JS = elm.js
JS_MIN = elm.min.js

# Elm sources and its entry point, Main.elm
SRCS = $(wildcard src/*.elm)
MAIN = src/Main.elm

# Index files, using either elm.js or elm.min.js
INDEX = custom_index.html
INDEX_MIN = custom_index_min.html

# Doc options: directory, index file, white-list of files to generate their
# documentation (leave it empty if you want all files in SRCS) and a license
# name for the elm-doc --fake-license option
DOC_DIR = doc
DOC_INDEX = $(DOC_DIR)/index.html
DOC_FILES =
DOC_FAKE_LICENSE = 'AGPL-3.0'
DOC_PORT = 8888

# Elm binary, elm make options and elm-live options
ELM_PATH = /home/alejandro/.node_modules/bin/elm
ELM_MAKE_OPTS = --debug --output=$(JS)
ELM_LIVE_HOST = localhost
ELM_LIVE_PORT = 8000
ELM_LIVE_OPTS = --open --pushstate --start-page=$(INDEX) --host $(ELM_LIVE_HOST) --port $(ELM_LIVE_PORT)

# Remote machine SSH credentials and remote directory where the web is served
REMOTE = alejandro@rpi
REMOTE_DIR = /home/alejandro/web

##############################################################################
######################### Generation of actual files #########################
##############################################################################

# Compile a debug elm.js file from all the sources
$(JS) : $(SRCS)
	elm make $(MAIN) $(ELM_MAKE_OPTS)

# Compile a production-ready, optimizied and minified elm.min.js
$(JS_MIN) : $(SRCS)
	./optimize.sh $(MAIN)

# Generate the documentation from all the sources
$(DOC_INDEX) : $(SRCS)
	elm-doc . --output $(DOC_DIR) \
	          --fake-license $(DOC_FAKE_LICENSE) \
	          --elm-path $(ELM_PATH) \
	          $(DOC_FILES)

# Generate an index file that uses the minified elm.js file
$(INDEX_MIN) : $(INDEX)
	sed 's/$(JS)/$(JS_MIN)/' $(INDEX) > $(INDEX_MIN)

##############################################################################
############################### PHONY targets ################################
##############################################################################

.PHONY: live debug prod deploy doc clean
.DEFAULT_GOAL := live

# Use elm-live to compile all the sources, watch their changes and recompile
# when a change is done, launching a server listening in localhost:8000
live : $(SRCS)
	elm-live $(MAIN) $(ELM_LIVE_OPTS) -- $(ELM_MAKE_OPTS)

# Compile all the sources into a debug elm.js file
debug : $(JS)

# Compile all the sources into a production-ready, minified elm.min.js file
prod : $(JS_MIN)

# Deploy the production-compiled project into the remote directory
deploy : $(JS_MIN) $(INDEX_MIN)
	ssh $(REMOTE) "cp -r $(REMOTE_DIR) $(REMOTE_DIR).backup"
	scp $(JS_MIN) $(REMOTE):$(REMOTE_DIR)/$(JS_MIN)
	scp $(INDEX_MIN) $(REMOTE):$(REMOTE_DIR)/index.html

# Generate all the documentation and launch a server to view it in
# localhost:$(DOC_PORT)
doc : $(DOC_INDEX)
	python -m http.server --directory $(DOC_DIR) $(DOC_PORT)

# Remove elm.js and elm.min.js files, as well as the whole doc directory
clean:
	rm -rf $(JS) $(JS_MIN) $(DOC_DIR)

SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v

all: $(TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

clean:
	rm -f $(TARGETS)

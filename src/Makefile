ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := ../include $(wildcard ../deps/*/include) $(CCETC_ROOT)/res/projects/
EBIN_DIRS := $(wildcard ../deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:%=-I %) $(EBIN_DIRS:%=-pa %)

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

EBIN_DIR := ../ebin
TEST_DIR := ../_test
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app)
MODULES = $(ERL_SOURCES:%.erl=%)

../ebin/%.app: %.app
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) -pa $(EBIN_DIR) $<

./%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o . $<

APPLICATION=nemo
DOC_OPTS={dir,\"../doc\"}
TEST_PLT=$(TEST_DIR)/dialyzer_plt

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

edoc:
	$(ERL) -noshell -pa ../ebin \
		-eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
		-s init stop

test: $(EBIN_FILES)
	mkdir -p $(TEST_DIR);
	@../support/run_tests.escript $(EBIN_DIR) | tee $(TEST_DIR)/test.log

$(TEST_PLT):
	mkdir -p $(TEST_DIR)
	cp $(DIALYZER_PLT) $(TEST_PLT)
	dialyzer --plt $(TEST_PLT) --add_to_plt -r ../deps/*/ebin

clean_plt:
	rm $(TEST_PLT)

dialyzer: $(TEST_PLT)
	dialyzer --src --plt $(TEST_PLT) -DNOTEST -DDIALYZER -c ../src | tee $(TEST_DIR)/dialyzer.log

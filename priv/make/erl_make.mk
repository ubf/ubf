#
# $Id: erl_make.mk 107291 2008-07-14 15:06:21Z norton $
#

######################################################################

ASNS ?=

MODULES ?=

PRIVS ?=
TESTS ?=
QUICKTESTS ?=

TICKTIME ?= 60

######################################################################
# erl/erlc
POSSIBLE__ERL1 ?= erl
POSSIBLE__ERLC1 ?= erlc

ifeq ($(origin ERL), undefined)
POSSIBLE__LS_OUT1=$(shell /bin/ls -l $(POSSIBLE__ERL1) 2> /dev/null)
ifneq (,$(findstring erl,$(POSSIBLE__LS_OUT1)))
	ERL=$(POSSIBLE__ERL1)
endif
endif
ifeq ($(origin ERLC), undefined)
POSSIBLE__LS_OUT1=$(shell /bin/ls -l $(POSSIBLE__ERLC1) 2> /dev/null)
ifneq (,$(findstring erl,$(POSSIBLE__LS_OUT1)))
	ERLC=$(POSSIBLE__ERLC1)
endif
endif

ERL ?= erl
ERLC ?= erlc
ERLC_EMULATOR = $(ERL)
ifeq ($(ERL), erl)
	export DIALYZER ?= dialyzer
else
	export DIALYZER ?= $(patsubst %/bin/erl,%/bin/dialyzer,$(ERL))
endif
export ERL
export ERLC
export ERLC_EMULATOR

ERLDIRNAME1 := $(shell which $(ERL))
ERLDIRNAME2 := $(patsubst %/bin/erl,%,$(ERLDIRNAME1))
ERLDIRNAME3 := $(notdir $(ERLDIRNAME2))

OTPVERSION ?= $(patsubst erlang%,%,$(ERLDIRNAME3))

ifndef USE_SPECS
# our type specs rely on features / bug fixes in dialyzer that are
# only available in R12B-3 upwards
# If we're using R12B, it's at least -5, so don't worry here until R13B is out.
ifeq ($(findstring R12B, $(OTPVERSION)),)
USE_SPECS=false
else
USE_SPECS=true
endif
endif

## well-known ERT applications
EDOCDIRNAME := $(wildcard $(ERLDIRNAME2)/lib/erlang/lib/edoc-*)
EIDIRNAME := $(wildcard $(ERLDIRNAME2)/lib/erlang/lib/erl_interface-*)
ERTSDIRNAME ?= $(wildcard $(ERLDIRNAME2)/lib/erlang/erts-*)
XMERLDIRNAME := $(wildcard $(ERLDIRNAME2)/lib/erlang/lib/xmerl-*)
EUNITDIRNAME := $(wildcard $(ERLDIRNAME2)/lib/erlang/lib/eunit-*)

EBIN = ../ebin
DOC_DIR = ../doc.$(shell cat ../.otpversion)
INCLUDE_DIR = ../include
PRIV_DIR = ../priv
TEST_DIR = ./Unit-Test-Files
EUNITTEST_DIR = ./Unit-EUnit-Files
QUICKTEST_DIR = ./Unit-Quick-Files
INCLUDE = -I$(PRIV_DIR) -I$(INCLUDE_DIR) -I$(EBIN) -I../ $(DEP_INCLUDES)

ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(INCLUDE) -pz $(EBIN) $(DEP_EBINS) $(ERL_FLAGS_Top)
ERL_COMPILE_FLAGS += $(shell if [ "$(USE_SPECS)" = "true" ] ; then echo "-Duse_specs"; fi )

TESTONLY_ERL_COMPILE_FLAGS := $(ERL_COMPILE_FLAGS) -I $(EUNITDIRNAME)/include +warn_obsolete_guard +debug_info

## now add DEBUG after TESTONLY has been constructed
ERL_COMPILE_FLAGS += $(DEBUG)

EMULATOR = beam

######################################################################
ASN_FILES = $(ASNS:%=%.asn)
TARGET_ASNS = $(ASNS:%=$(EBIN)/%.$(EMULATOR))

ERL_FILES = $(MODULES:%=%.erl)
TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))
TARGET_PFILES = $(MODULES:%=$(EBIN)/%.P)
TARGET_DFILES = $(MODULES:%=$(EBIN)/%.d)

ERL_FILES1 = $(MODULES1:%=%.erl)
TARGET_FILES1 = $(MODULES1:%=$(EBIN)/%.$(EMULATOR))
TARGET_PFILES1 = $(MODULES1:%=$(EBIN)/%.P)
TARGET_DFILES1 = $(MODULES1:%=$(EBIN)/%.d)

ERL_FILES2 = $(MODULES2:%=%.erl)
TARGET_FILES2 = $(MODULES2:%=$(EBIN)/%.$(EMULATOR))
TARGET_PFILES2 = $(MODULES2:%=$(EBIN)/%.P)
TARGET_DFILES2 = $(MODULES2:%=$(EBIN)/%.d)

ERL_PRIVS = $(PRIVS:%=%.erl)
TARGET_PRIVS = $(PRIVS:%=$(PRIV_DIR)/%.$(EMULATOR))
TARGET_PPRIVS = $(PRIVS:%=$(PRIV_DIR)/%.P)
TARGET_DPRIVS = $(PRIVS:%=$(PRIV_DIR)/%.d)

ERL_TESTS = $(TESTS:%=%.erl)
TARGET_TESTS = $(TESTS:%=$(TEST_DIR)/%.$(EMULATOR))
TARGET_PTESTS = $(TESTS:%=$(TEST_DIR)/%.P)
TARGET_DTESTS = $(TESTS:%=$(TEST_DIR)/%.d)

ERL_EUNITTESTS = $(EUNITTESTS:%=%.erl)
TARGET_EUNITTESTS = $(EUNITTESTS:%=$(EUNITTEST_DIR)/%.$(EMULATOR))
TARGET_PEUNITTESTS = $(EUNITTESTS:%=$(EUNITTEST_DIR)/%.P)
TARGET_DEUNITTESTS = $(EUNITTESTS:%=$(EUNITTEST_DIR)/%.d)

ERL_QUICKTESTS = $(QUICKTESTS:%=%.erl)
TARGET_QUICKTESTS = $(QUICKTESTS:%=$(QUICKTEST_DIR)/%.$(EMULATOR))
TARGET_PQUICKTESTS = $(QUICKTESTS:%=$(QUICKTEST_DIR)/%.P)
TARGET_DQUICKTESTS = $(QUICKTESTS:%=$(QUICKTEST_DIR)/%.d)

###################################
# EBIN
$(EBIN)/%.$(EMULATOR): %.asn
	$(ERLC) $(ERL_FLAGS) $(ERL_FLAGS_SE) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

$(EBIN)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_FLAGS_SE) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

$(EBIN)/%.P: %.erl
	-@set -e; rm -f $@; \
	($(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) \
		-W0 -P $< \
		> /dev/null 2>&1 || touch $@)

$(EBIN)/%.d: $(EBIN)/%.P
	-@set -e; rm -f $@; \
	(egrep -- '-file\(.*\.h..' $< | sort -u | \
		perl -pe 's/^.*\-file\("(.+)\.h(..).*/$$1.h$$2/;' | \
		xargs echo $@ $(@:%.d=%.$(EMULATOR)) : > $@ || touch $@)

###################################
# PRIV_DIR
$(PRIV_DIR)/%.$(EMULATOR): $(PRIV_DIR)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(PRIV_DIR) $<

$(PRIV_DIR)/%.P: $(PRIV_DIR)/%.erl
	-@set -e; rm -f $@; \
	($(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(PRIV_DIR) \
		-W0 -P $< \
		> /dev/null 2>&1 || touch $@)

$(PRIV_DIR)/%.d: $(PRIV_DIR)/%.P
	-@set -e; rm -f $@; \
	(egrep -- '-file\(.*\.h..' $< | sort -u | \
		perl -pe 's/^.*\-file\("(.+)\.h(..).*/$$1.h$$2/;' | \
		xargs echo $@ $(@:%.d=%.$(EMULATOR)) : > $@ || touch $@)

###################################
# TEST_DIR
$(TEST_DIR)/%.$(EMULATOR): $(TEST_DIR)/%.erl
	$(ERLC) $(ERL_FLAGS) $(TESTONLY_ERL_COMPILE_FLAGS) -o $(TEST_DIR) $<

$(TEST_DIR)/%.P: $(TEST_DIR)/%.erl
	-@set -e; rm -f $@; \
	($(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(TEST_DIR) \
		-W0 -P $< \
		> /dev/null 2>&1 || touch $@)
	touch $@

$(TEST_DIR)/%.d: $(TEST_DIR)/%.P
	-@set -e; rm -f $@; \
	(egrep -- '-file\(.*\.h..' $< | sort -u | \
		perl -pe 's/^.*\-file\("(.+)\.h(..).*/$$1.h$$2/;' | \
		xargs echo $@ $(@:%.d=%.$(EMULATOR)) : > $@ || touch $@)

###################################
# EUNITTEST_DIR
$(EUNITTEST_DIR)/%.$(EMULATOR): $(EUNITTEST_DIR)/%.erl
	$(ERLC) $(ERL_FLAGS) $(TESTONLY_ERL_COMPILE_FLAGS) -o $(EUNITTEST_DIR) $<

$(EUNITTEST_DIR)/%.P: $(EUNITTEST_DIR)/%.erl
	-@set -e; rm -f $@; \
	($(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EUNITTEST_DIR) \
		-W0 -P $< \
		> /dev/null 2>&1 || touch $@)

$(EUNITTEST_DIR)/%.d: $(EUNITTEST_DIR)/%.P
	-@set -e; rm -f $@; \
	(egrep -- '-file\(.*\.h..' $< | sort -u | \
		perl -pe 's/^.*\-file\("(.+)\.h(..).*/$$1.h$$2/;' | \
		xargs echo $@ $(@:%.d=%.$(EMULATOR)) : > $@ || touch $@)

###################################
# QUICKTEST_DIR
$(QUICKTEST_DIR)/%.$(EMULATOR): $(QUICKTEST_DIR)/%.erl
	$(ERLC) $(ERL_FLAGS) $(TESTONLY_ERL_COMPILE_FLAGS) -o $(QUICKTEST_DIR) $<

$(QUICKTEST_DIR)/%.P: $(QUICKTEST_DIR)/%.erl
	-@set -e; rm -f $@; \
	($(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(QUICKTEST_DIR) \
		-W0 -P $< \
		> /dev/null 2>&1 || touch $@)

$(QUICKTEST_DIR)/%.d: $(QUICKTEST_DIR)/%.P
	-@set -e; rm -f $@; \
	(egrep -- '-file\(.*\.h..' $< | sort -u | \
		perl -pe 's/^.*\-file\("(.+)\.h(..).*/$$1.h$$2/;' | \
		xargs echo $@ $(@:%.d=%.$(EMULATOR)) : > $@ || touch $@)

######################################################################
# LD_LIBRARY_PATH

LD_LIBRARY_PATH:=$(DEP_LIBS):$(ERLDIRNAME2)/openssl/lib:$(LD_LIBRARY_PATH)
export LD_LIBRARY_PATH

######################################################################
# ALL1
ALL1 =
ALL1 += $(TARGET_ASNS)

# ALL2
ALL2 =
ALL2 += $(TARGET_DFILES) $(TARGET_PFILES)
ALL2 += $(TARGET_DPRIVS) $(TARGET_PPRIVS)
ALL2 += $(TARGET_DTESTS) $(TARGET_PTESTS)
ALL2 += $(TARGET_DEUNITTESTS) $(TARGET_PEUNITTESTS)
ALL2 += $(TARGET_DQUICKTESTS) $(TARGET_PQUICKTESTS)

# ALL3
ALL3 =
ALL3 += $(TARGET_FILES)
ALL3 += $(TARGET_PRIVS)
ALL3 += $(TARGET_TESTS)
ALL3 += $(TARGET_EUNITTESTS)
ALL3 += $(TARGET_QUICKTESTS)

# ALL4
ALL4 =

# all
all:: $(ALL1) $(ALL2) $(ALL3) $(ALL4)
ifneq (,$(TARGET_DFILES))
-include $(TARGET_DFILES)
endif
ifneq (,$(TARGET_DFILES1))
-include $(TARGET_DFILES1)
endif
ifneq (,$(TARGET_DFILES2))
-include $(TARGET_DFILES2)
endif
ifneq (,$(TARGET_DPRIVS))
-include $(TARGET_DPRIVS)
endif
ifneq (,$(TARGET_DTESTS))
-include $(TARGET_DTESTS)
endif
ifneq (,$(TARGET_DEUNITTESTS))
-include $(TARGET_DEUNITTESTS)
endif
ifneq (,$(TARGET_DQUICKTESTS))
-include $(TARGET_DQUICKTESTS)
endif

clean::
	rm -f $(EBIN)/*.$(EMULATOR) $(PRIV_DIR)/*.$(EMULATOR) $(TEST_DIR)/*.$(EMULATOR) $(EUNITTEST_DIR)/*.$(EMULATOR) $(QUICKTEST_DIR)/*.$(EMULATOR)
	rm -f $(EBIN)/*.{P,d,huc} $(PRIV_DIR)/*.{P,d,huc} $(TEST_DIR)/*.{P,d,huc} $(EUNITTEST_DIR)/*.{P,d,huc} $(QUICKTEST_DIR)/*.{P,d,huc}
	rm -f core core.* *.core erl_crash.dump *.boot *.script *.log

##
## NOTE: Dialyzer's behavior has changed since R11B-5.  Let's generate
## the plt manually until there is time to investigate how to best
## integrate and automate this process for nightly builds, developers,
## etc.
##
## The following command will construct a new PLT table in the file
## ~/.dialyzer_plt
##
## $ dialyzer --build_plt -r /usr/local/gemini/ert/R12B-5/lib/erlang/lib/{compiler-*,crypto-*,edoc-*,inets-*,kernel-*,mnesia-*,odbc-*,parsetools-*,sasl-*,stdlib-*,tools-*,xmerl-*}/ebin
##

##
## TODO: Figure out how to fix the no_return and no_unused warnings in
## the source code
##

## NOTE: exclude src/erl-third-party/quviq/eqc since we only have beam
## files
run-dialyzer: all
	-$(DIALYZER) --verbose \
		-Wno_return \
		-Wno_unused \
		-r $(EBIN) $(wildcard $(TEST_DIR)) $(wildcard $(EUNITTEST_DIR)) $(wildcard $(QUICKTEST_DIR)) \
		$(subst -pz,,$(DEP_EBINS_NOEQC) $(wildcard $(DEP_TESTS)) $(wildcard $(DEP_EUNITTESTS)) $(wildcard $(DEP_QUICKTESTS))) \
		> dialyzer.log

run-dialyzer-interactive: all
	-@set -e; (sleep 4 ; \
	 echo "\r\n" ; \
	 echo "\r\n" ; \
	 echo "In the lower-left window, click on the 'src' dir.\r\n" ; \
	 echo "Then click the 'Add recursively' button, then 'Run'.\r\n" ; \
	 echo "\r\n") &
	( cd ../../../../.. ; $(DIALYZER) )


PRERUNAPP= \
	env ERL_MAX_ETS_TABLES=10007 \
	$(ERL) \
		+A 64 +K true -smp auto +Mis true
POSTRUNAPP= \
	-pz $(EBIN) \
	$(DEP_EBINS) \
	-kernel net_ticktime $(TICKTIME) \
	-config ../priv/sys \
	-central_config ../priv/central.conf \
	-ticket_broker_config ../priv/broker.conf

RUNERL1= \
	$(PRERUNAPP) \
	-sname runerl1 \
	$(POSTRUNAPP) \
	 -pz $(TEST_DIR) -pz $(EUNITTEST_DIR) -pz $(QUICKTEST_DIR) $(DEP_TESTS) $(DEP_EUNITTESTS) $(DEP_QUICKTESTS)

RUNERL2= \
	$(PRERUNAPP) \
	-sname runerl2 \
	$(POSTRUNAPP) \
	 -pz $(TEST_DIR) -pz $(EUNITTEST_DIR) -pz $(QUICKTEST_DIR) $(DEP_TESTS) $(DEP_EUNITTESTS) $(DEP_QUICKTESTS)

run-erl1: all
	$(RUNERL1)

run-erl1-debugger: all
	$(RUNERL1) -s debugger start

run-erl2: all
	$(RUNERL2)

run-erl2-debugger: all
	$(RUNERL2) -s debugger start

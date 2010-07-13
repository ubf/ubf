###----------------------------------------------------------------------
### Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
###
### Licensed under the Apache License, Version 2.0 (the "License");
### you may not use this file except in compliance with the License.
### You may obtain a copy of the License at
###
###     http://www.apache.org/licenses/LICENSE-2.0
###
### Unless required by applicable law or agreed to in writing, software
### distributed under the License is distributed on an "AS IS" BASIS,
### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
### See the License for the specific language governing permissions and
### limitations under the License.
###
### File    : Makefile
### Purpose : 
###----------------------------------------------------------------------

MODULES = \
	contract_parser \
	contract_lex \
	contract_yecc \
	ubf_client \
	ubf_server \
	ubf_plugin_handler \
	proc_socket_server \
	proc_utils \
	contracts \
	contracts_abnf \
	contract_driver \
	contract_proto \
	contract_manager \
	contract_manager_tlog \
	ubf \
	ubf_utils \
	ubf_driver \
	ebf \
	ebf_driver \
	ubf_plugin_meta_stateful \
	ubf_plugin_meta_stateless \
	ubf_plugin_stateful \
	ubf_plugin_stateless

PRIVS = \

TESTS = \
	bug \
	file_client \
	file_plugin \
	irc_client \
	irc_client_gs \
	irc_plugin \
	socket_test_client \
	socket_test_server \
	test_plugin \
	test_app \
	test_sup \
	test_ubf \
	test_ebf \
	test_etf \
	ubf_test

EUNITTESTS = \
	types_plugin \
	stateless_plugin \
	stateless_plugin_app \
	stateless_plugin_sup \
	stateless_plugin_test \
	stateful_plugin \
	stateful_plugin_app \
	stateful_plugin_sup \
	stateful_plugin_test

PLUGINS = \
	ubf_plugin_meta_stateful \
	ubf_plugin_meta_stateless

TESTPLUGINS = \
	$(TEST_DIR)/file_plugin \
        $(TEST_DIR)/irc_plugin \
        $(TEST_DIR)/test_plugin

EUNITTESTPLUGINS = \
	$(EUNITTEST_DIR)/types_plugin \
	$(EUNITTEST_DIR)/stateless_plugin \
	$(EUNITTEST_DIR)/stateful_plugin

# include erl make
include $(shell cat ../.depends-gmt-bom)/make/erl_make.mk

# standard targets
clean::
	rm -f contract_lex.erl contract_yecc.erl

check: all
	-rm -f ./*.log
	$(RUNERL1) -noinput -noshell -pz $(TEST_DIR) -pz $(EUNITTEST_DIR) \
		-s ubf_test tests \
		-s test_ubf tests \
		-s test_ebf tests \
		-s test_etf tests \
		-s stateless_plugin_test do_eunit \
		-s stateful_plugin_test do_eunit \
		-s erlang halt \
		> ./check.log

# special targets - ubf
$(TARGET_PLUGINS): \
	$(EBIN)/contract_parser.$(EMULATOR) \
	$(EBIN)/contract_lex.$(EMULATOR) \
	$(EBIN)/contract_yecc.$(EMULATOR)

contract_lex.erl: contract_lex.xrl $(EBIN)/contract_parser.$(EMULATOR)
	$(ERL) -noinput -noshell -pz $(EBIN) \
		-s contract_parser make_lex \
		-s erlang halt

contract_yecc.erl: contract_yecc.yrl $(EBIN)/contract_parser.$(EMULATOR)
	$(ERL) -noinput -noshell -pz $(EBIN) \
		-s contract_parser make_yecc \
		-s erlang halt

# irc demo
irc-server: all
	-rm -f ./*.log
	$(RUNERL1) -noinput -noshell -pz $(TEST_DIR) \
		-eval "server:start(2000)."

irc-client1: all
	$(ERL) -pz $(EBIN) -pz $(TEST_DIR) \
		-s irc_client_gs batch user1

irc-client2: all
	$(ERL) -pz $(EBIN) -pz $(TEST_DIR) \
		-s irc_client_gs batch user2

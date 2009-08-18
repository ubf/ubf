#
# $Id: Makefile 131597 2009-04-09 06:34:51Z norton $
#

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
	contract_manager \
	ubf \
	ubf_utils \
	ubf_driver \
	jsf \
	jsf_utils \
	jsf_driver \
	ebf_driver \
	ubf_plugin_meta_serverful \
	ubf_plugin_meta_serverless

PRIVS = \
	leex

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
	test_jsf \
	ubf_test \
	jsf_test \
	jsf_test2

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
	ubf_plugin_meta_serverful \
	ubf_plugin_meta_serverless

TESTPLUGINS = \
	$(TEST_DIR)/file_plugin \
        $(TEST_DIR)/irc_plugin \
        $(TEST_DIR)/test_plugin

EUNITTESTPLUGINS = \
	$(EUNITTEST_DIR)/types_plugin \
	$(EUNITTEST_DIR)/stateless_plugin \
	$(EUNITTEST_DIR)/stateful_plugin

MY_ERLANG_RFC4627_EBIN = ./example-symlink-to-erlang-rfc4627-ebin

# include erl make
include ../priv/make/erl_make.mk

# standard targets
clean::
	rm -f contract_lex.erl contract_yecc.erl
	rm -f contract_lex.$(EMULATOR) contract_yecc.$(EMULATOR)

check: all
	-rm -f ./*.log
	$(RUNERL1) -noinput -noshell -pz $(TEST_DIR) -pz $(EUNITTEST_DIR) \
		-s ubf_test tests \
		-s test_ubf tests \
		-s test_ebf tests \
		-s test_etf tests \
		-s stateless_plugin_test do_eunit \
		-s erlang halt \
		> ./check.log

check-jsf: all
	$(RUNERL1) -noinput -noshell -pz $(TEST_DIR) \
		-pz $(MY_ERLANG_RFC4627_EBIN) \
		-s jsf_test tests \
		-s jsf_test2 tests \
		-s test_jsf tests \
		-s erlang halt \
		>> ./check.log

# special targets - ubf
${TESTPLUGINS:%=$(TEST_DIR)/%.$(EMULATOR)}: \
	${TESTPLUGINS:%=$(TEST_DIR)/%.con}

${EUNITTESTPLUGINS:%=$(EUNITTEST_DIR)/%.$(EMULATOR)}: \
	${TESTPLUGINS:%=$(TEST_DIR)/%.con}

$(TARGET_PLUGINS): \
	$(EBIN)/contracts.$(EMULATOR) \
	$(EBIN)/contract_parser.$(EMULATOR) \
	$(EBIN)/contract_lex.$(EMULATOR) \
	$(EBIN)/contract_yecc.$(EMULATOR)

.yrl.erl:
	echo "yecc:yecc($*,$*), halt()." | $(ERL) -noshell

contract_lex.erl: contract.xrl $(EBIN)/contract_parser.$(EMULATOR) $(PRIV_DIR)/leex.$(EMULATOR)
	$(ERL) -noinput -noshell -pz $(EBIN) -pa $(PRIV_DIR) \
		-s contract_parser make_lex \
		-s erlang halt

contract_yecc.erl: contract.yrl $(EBIN)/contract_parser.$(EMULATOR)
	$(ERL) -noinput -noshell -pz $(EBIN) -pa $(PRIV_DIR) \
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

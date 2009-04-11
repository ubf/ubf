.SUFFIXES:  .beam .hrl .erl .html .ehtml .fig .gif .yrl .con .buc

.erl.beam: 
	erlc -W $<

.con.buc: contracts.beam
	./conc $<

.yrl.erl:
	echo "yecc:yecc($*,$*), halt()." | erl

ERL =   ubf proc_socket_server find server\
	test ubf_driver ubf_test file_client\
	contracts contract_manager client plugin_handler\
	leex contract_lex contract_yecc irc_client ubf_utils

PLUGINS = test_plugin file_plugin irc_plugin server_plugin

CONTRACTS = test_plugin file_plugin irc_plugin server server_plugin

BEAM = ${ERL:%=%.beam}
BUC = ${CONTRACTS:%=%.buc}
CON = ${CONTRACTS:%=%.con}
PLUG = ${PLUGINS:%=%.beam}

# make test (tests)
# make server (starts server)

all: ${BEAM} ${BUC} ${PLUG}

test: ${BEAM} ${BUC} ${PLUG}
	erl -noshell -pa ${HOME}"/p2p/new" -s test tests

server: ${BEAM} ${BUC} ${PLUG}
	erl -noshell -pa ${HOME}"/p2p/new" -s test ss


contract_lex.erl: contract.xrl contract_parser.beam leex.beam
	erl -noshell -s contract_parser make_lex -s erlang halt

contract_yecc.erl: contract.yrl contract_parser.beam 
	erl -noshell -s contract_parser make_yecc -s erlang halt

${PLUG}: contract_parser.beam contract_lex.beam contract_yecc.beam

clean:
	rm -f ${BEAM} ${BUC} ${PLUG} contract_lex.erl contract_yecc.erl
	rm -f contract_parser.beam erl_crash.dump


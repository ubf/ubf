%% @doc Generic protocol encoder/decoder for protocol sessions.
%%
%% <img src="../priv/doc/ubf-flow-01.png"></img>

-module(contract_proto).

%% Interface Functions
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{proto_vsn,0}
     , {proto_driver,0}
     , {proto_packet_type,0}
     , {decode_init,0}
     , {decode,1}
     , {decode,2}
     , {encode,1}
     , {encode,2}
    ].

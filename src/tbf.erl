%% @doc Functions for Thrift(Binary)&lt;->Erlang data conversion.
%%
%% For most purposes, these functions are not called by code outside
%% of this library: Erlang client &amp; Erlang server application code
%% usually have no need to use these functions.
%%
%% == Links ==
%%
%% <ul>
%% <li> http://incubator.apache.org/thrift </li>
%% </ul>
%%
%% == Thrift Basic Types (ABNF) ==
%% ```
%% message        =  message-begin struct message-end
%% message-begin  =  method-name message-type message-seqid
%% message-end    =  ""
%% method-name    =  STRING
%% message-type   =  T-CALL/ T-REPLY/ T-EXCEPTION/ T-ONEWAY
%% message-seqid  =  I32
%%
%% struct         =  struct-begin *field field-stop struct-end
%% struct-begin   =  struct-name
%% struct-end     =  ""
%% struct-name    =  STRING
%% field-stop     =  T-STOP
%%
%% field          =  field-begin field-data field-end
%% field-begin    =  field-name field-type field-id
%% field-end      =  ""
%% field-name     =  STRING
%% field-type     =  T-STOP/ T-VOID/ T-BOOL/ T-BYTE/ T-I08/ T-I16/ T-I32/ T-U64/ T-I64/ T-DOUBLE/
%%                   T-STRING/ T-BINARY/ T-STRUCT/ T-MAP/ T-SET/ T-LIST
%% field-id       =  I16
%% field-data     =  BOOL/ I08/ I16/ I32/ U64/ I64/ DOUBLE/ STRING/ BINARY/
%%                   struct/ map/ list/ set
%% field-datum    =  field-data field-data
%%
%% map            =  map-begin *field-datum map-end
%% map-begin      =  map-key-type map-value-type map-size
%% map-end        =  ""
%% map-key-type   =  field-type
%% map-value-type =  field-type
%% map-size       =  I32
%%
%% list           =  list-begin *field-data list-end
%% list-begin     =  list-elem-type list-size
%% list-end       =  ""
%% list-elem-type =  field-type
%% list-size      =  I32
%%
%% set            =  set-begin *field-data set-end
%% set-begin      =  set-elem-type set-size
%% set-end        =  ""
%%
%% set-elem-type  =  field-type
%% set-size       =  I32
%%
%% '''
%%
%% == Thrift (Binary) Core Types (ABNF) ==
%% ```
%% BOOL           =  %x00/ %x01         ; 8/big-signed
%% BYTE           =  OCTET              ; 8/big-signed
%% I08            =  OCTET              ; 8/big-signed
%% I16            =  2*OCTET            ; 16/big-signed
%% I32            =  4*OCTET            ; 32/big-signed
%% U64            =  8*OCTET            ; 64/big-unsigned
%% I64            =  8*OCTET            ; 64/big-signed
%% DOUBLE         =  8*OCTET            ; 64/big-signed-float
%% STRING         =  I32 UTF8-octets
%% BINARY         =  I32 *OCTET
%%
%% T-CALL         =  %x01
%% T-REPLY        =  %x02
%% T-EXCEPTION    =  %x03
%% T-ONEWAY       =  %x04
%%
%% T-STOP         =  %x00
%% T-VOID         =  %x01
%% T-BOOL         =  %x02
%% T-BYTE         =  %x03
%% T-I08          =  %x05
%% T-I16          =  %x06
%% T-I32          =  %x08
%% T-U64          =  %x09
%% T-I64          =  %x0a
%% T-DOUBLE       =  %x04
%% T-STRING       =  %x0b
%% T-BINARY       =  %x0b
%% T-STRUCT       =  %x0c
%% T-MAP          =  %x0d
%% T-SET          =  %x0e
%% T-LIST         =  %x0f
%%
%% '''
%%
%% == Mapping: Thrift Types (Erlang) ==
%% ```
%% tbf::message() = {'message', tbf::method_name(), tbf::message_type(), tbf::message_seqid(), tbf::struct()}.
%% tbf::method_name() = binary().
%% tbf::message_type() = 'call' | 'reply' | 'exception' | 'oneway'.
%% tbf::message_seqid() = integer().
%%
%% tbf::struct() = {'struct', tbf::struct_name(), [tbf::field()]}.
%% tbf::struct_name() = binary().
%%
%% tbf::field() = {'field', tbf::field_name(), tbf::field_type(), tbf::field_id(), tbf::field_data()}.
%% tbf::field_name() = binary().
%% tbf::field_type() = 'stop' | 'void' | 'bool' | 'byte'
%%                   | 'i08' | 'i16' | 'i32' | 'u64' | 'i64' | 'double'
%%                   | 'string' | 'binary' | 'struct' | 'map' | 'set' | 'list'.
%% tbf::field_id() = integer().
%% tbf::field_data() = boolean() | integer() | double() | binary() | binary() |
%%                   | tbf::struct() | tbf::map() | tbf::set() | tbf::list().
%%
%% tbf::map() = {'map', tbf::map_type(), [tbf::map_data()]}.
%% tbf::map_type() = {tbf::field_type(), tbf::field_type()}.
%% tbf::map_data() = {tbf::field_data(), tbf::field_data()}.
%%
%% tbf::list() = {'list', tbf::list_type(), [tbf::list_data()]}.
%% tbf::list_type() = tbf::field_type().
%% tbf::list_data() = tbf::field_data().
%%
%% tbf::set() = {'set', tbf::set_type(), [tbf::set_data()]}.
%% tbf::set_type() = tbf::field_type().
%% tbf::set_data() = tbf::field_data().
%%
%% tbf::boolean() = 'true' | 'false'.
%%
%% '''
%%
%% == Mapping: UBF Types (Erlang) ==
%% ```
%% ubf::tuple() = tuple().
%%
%% ubf::list() = list().
%%
%% ubf::number = integer() | float().
%%
%% ubf::string() = {'$S', [integer()]}.
%%
%% ubf::proplist() = {'$P', [{term(), term()}]}.
%%
%% ubf::binary() = binary().
%%
%% ubf::boolean() = 'true' | 'false'.
%%
%% ubf::atom() = atom().
%%
%% ubf::record() = record().
%%
%% ubf::term() = ubf::tuple() | ubf::list() | ubf::number()
%%             | ubf::string() | ubf::proplist() | ubf::binary()
%%             | ubf::boolean() | ubf::atom() | ubf::record().
%%
%% ubf::state() = ubf::atom().
%%
%% ubf::request() = ubf::term().
%%
%% ubf:response() = {Reply::term(), NextState::state()}.
%%
%% '''
%%
%% == UBF Messages ==
%% ```
%% Remote Procedure Call (Client -> Server -> Client)
%%   ubf::request() => ubf::response().
%%
%% Asynchronous Event (Server -> Client)
%%   'EVENT' => ubf::term().
%%
%% Asynchronous Event (Server <- Client)
%%   'EVENT' <= ubf::term().
%%
%% '''
%%
%% == Mapping: Thrift Messages&lt;->UBF Messages ==
%% ```
%% Remote Procedure Call (Client -> Server -> Client)
%%  ubf::request() = {tbf::method_name(), tbf::message_seqid(), tbf::struct()}.
%%  ubf::response() = {'reply', tbf::struct()} | {'exception', tbf::struct()}.
%%
%% Asynchronous Event (Server -> Client)
%%   ubf:term() = {tbf::method_name(), tbf::message_seqid(), tbf::struct()}.
%%
%% Asynchronous Event (Server <- Client)
%%   ubf:term() = {tbf::method_name(), tbf::message_seqid(), tbf::struct()}.
%%
%% '''
%%
%%  NOTE: message_type() is not passed to the Erlang server
%%  implementation.
%%
%%  NOTE: Thrift has no concept of a 'state name' so it is not
%%  returned to the Thrift client and/or the Erlang server.
%%
%%  TBD: How to handle the following error cases?
%%  <ul>
%%  <li> encoding/decoding errors </li>
%%  <li> server breaks contract </li>
%%  <li> client breaks contract </li>
%%  </ul>
%%
%%

-module(tbf).

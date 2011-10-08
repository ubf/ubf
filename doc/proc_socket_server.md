

#Module proc_socket_server#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Keeps track of a number of TCP sessions.

<a name="description"></a>

##Description##




This module will manage a collection of TCP sessions for the same
server.  If `Port` is 0, the underlying OS assigns an available
port number.  If a `Name` is not specified, the server will be
named `picoSocketServer_` ++ the TCP port number that the service
listens to, e.g. `picoSocketServer_9923`.



A managed server can be started, stopped, enumerate child sessions,  
and limit the maximum number of child sessions.

The conventions used by this module look quite different than
OTP-based services, due to its origin.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cold_start-7">cold_start/7</a></td><td></td></tr><tr><td valign="top"><a href="#server_children-1">server_children/1</a></td><td></td></tr><tr><td valign="top"><a href="#server_children-2">server_children/2</a></td><td></td></tr><tr><td valign="top"><a href="#server_port-1">server_port/1</a></td><td></td></tr><tr><td valign="top"><a href="#server_port-2">server_port/2</a></td><td></td></tr><tr><td valign="top"><a href="#server_status-1">server_status/1</a></td><td></td></tr><tr><td valign="top"><a href="#server_status-2">server_status/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_child-3">start_child/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_raw_server-5">start_raw_server/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_raw_server-7">start_raw_server/7</a></td><td></td></tr><tr><td valign="top"><a href="#start_server-3">start_server/3</a></td><td>Start a new UBF contract-using server.</td></tr><tr><td valign="top"><a href="#start_server-4">start_server/4</a></td><td></td></tr><tr><td valign="top"><a href="#stop_server-1">stop_server/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="cold_start-7"></a>

###cold_start/7##




`cold_start(Parent, Name, Port, Max, Fun, PacketType, PacketSize) -> any()`

<a name="server_children-1"></a>

###server_children/1##




`server_children(Name) -> any()`

<a name="server_children-2"></a>

###server_children/2##




`server_children(Pid, Timeout) -> any()`

<a name="server_port-1"></a>

###server_port/1##




`server_port(Name) -> any()`

<a name="server_port-2"></a>

###server_port/2##




`server_port(Pid, Timeout) -> any()`

<a name="server_status-1"></a>

###server_status/1##




`server_status(Name) -> any()`

<a name="server_status-2"></a>

###server_status/2##




`server_status(Pid, Timeout) -> any()`

<a name="start_child-3"></a>

###start_child/3##




`start_child(Parent, Listen, Fun) -> any()`

<a name="start_raw_server-5"></a>

###start_raw_server/5##




`start_raw_server(Port, Max, Fun, PacketType, PacketSize) -> any()`

<a name="start_raw_server-7"></a>

###start_raw_server/7##




`start_raw_server(Name, Port, Max, SpawnOpts, Fun, PacketType, PacketSize) -> any()`

<a name="start_server-3"></a>

###start_server/3##




`start_server(Port, Max, Fun) -> any()`





Start a new UBF contract-using server.


* This server accepts up to Max connections on TCP port Port

* SpawnOpts are the erlang garbage collection options for the spawned process.

* Each time a new connection is made, Fun(Socket) is called.





Fun = fun(Socket) -> exit(normal) | exit(socket_closed) |                       
exit({socket_error, Reason}) | exit(timeout).  
This fun will handle all of the protocol communication for a single  
TCP session.

A raw server uses packet length 0 (see start_raw_server/5 and
start_raw_server/7).<a name="start_server-4"></a>

###start_server/4##




`start_server(Name, Port, Max, Fun) -> any()`

<a name="stop_server-1"></a>

###stop_server/1##




`stop_server(Pid) -> any()`




#Module ubf_client#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


UBF client-side public API.

<a name="description"></a>

##Description##


This module implements most of the commonly-used client-side
functions required to talk to UBF servers:

* connect() to a UBF server

* rpc() to make a synchronous call to a connected UBF server

* stop() a connection

* install_handler() to add a callback function to handle
asynchronous notifications from theUBF server to your
client process.





Note that this library can support UBF(A), EBF, JSF, TBF, PBF, and
ABF transport.  See the `connect()` function arguments for   
details.



This module also provides an alternative client-side function for
calling's UBF contract manager and a UBF contract's implementation
without any side-effects: `lpc()` to make a synchronous local   
procedure call to a contract's implementation.

See the documentation for the `TBD` module for extra
commentary on writing an UBF server implementation module.

<a name="types"></a>

##Data Types##




###<a name="type-address">address()</a>##



<pre>address() = string() | [ip_address()](#type-ip_address)</pre>


A DNS hostname or IP address.


###<a name="type-connect_options">connect_options()</a>##



<pre>connect_options() = [{proto, ubf | ebf | jsf | tbf | pbf | abf}]</pre>


An OTP-style property list, see 'proplists' module for details.


###<a name="type-ip_address">ip_address()</a>##



<pre>ip_address() = string() | tuple()</pre>


An IP address in string form,
e.g. "127.0.0.1" (IPv4) or "::1" (IPv6), or in tuple form (see
documentation for Erlang's 'inet' module for details).


###<a name="type-plugin_module_list">plugin_module_list()</a>##



<pre>plugin_module_list() = [atom()]</pre>


A list of plugin module names
that will be passed to ubf_plugin_meta_stateful:new() or
ubf_plugin_meta_stateless:new() for client initialization.


###<a name="type-tcp_port">tcp_port()</a>##



<pre>tcp_port() = integer()</pre>


A TCP port number.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td>Connect to a UBF server at address Host + TCP port Port.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td>Connect to a UBF server at address Host + TCP port Port.</td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td>Connect to a UBF server at address Host + TCP port Port, or at       
pid/registered name Server.</td></tr><tr><td valign="top"><a href="#install_default_handler-1">install_default_handler/1</a></td><td>Install a default handler function (callback-style) for  
asynchronous UBF messages.</td></tr><tr><td valign="top"><a href="#install_handler-2">install_handler/2</a></td><td>Install a handler function (callback-style) for asynchronous  
UBF messages.</td></tr><tr><td valign="top"><a href="#lpc-2">lpc/2</a></td><td>Perform a synchronous LPC (local procedure) call with the
state 'none'.</td></tr><tr><td valign="top"><a href="#lpc-3">lpc/3</a></td><td>Perform a synchronous LPC (local procedure) call with the
specified state.</td></tr><tr><td valign="top"><a href="#lpc-4">lpc/4</a></td><td></td></tr><tr><td valign="top"><a href="#rpc-2">rpc/2</a></td><td>Perform a synchronous RPC call.</td></tr><tr><td valign="top"><a href="#rpc-3">rpc/3</a></td><td>Perform a synchronous RPC call.</td></tr><tr><td valign="top"><a href="#sendEvent-2">sendEvent/2</a></td><td>Send an asynchronous UBF message.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop a UBF client process.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="connect-2"></a>

###connect/2##




<pre>connect(Host::[address()](#type-address), Port::[tcp_port()](#type-tcp_port)) -&gt; {ok, pid(), [service()](#type-service)} | {error, term()}</pre>
<br></br>




Connect to a UBF server at address Host + TCP port Port.<a name="connect-3"></a>

###connect/3##




<pre>connect(Host::[address()](#type-address), Port::[tcp_port()](#type-tcp_port), Timeout::timeout()) -&gt; {ok, pid(), [service()](#type-service)} | {error, term()}</pre>
<br></br>




Connect to a UBF server at address Host + TCP port Port.<a name="connect-4"></a>

###connect/4##




<pre>connect(X::[address()](#type-address) | [plugin_module_list()](#type-plugin_module_list), Y::[tcp_port()](#type-tcp_port) | pid() | atom(), Options::[proplist()](#type-proplist), Timeout::timeout()) -&gt; {ok, pid(), [service()](#type-service)} | {error, term()}</pre>
<br></br>






Connect to a UBF server at address Host + TCP port Port, or at       
pid/registered name Server.

When using the alternate form, the first two arguments are:

1. Plugins: a plugin_module_list().

1. Server: either a process id (pid()) or process registered
name (atom()) for an already-started UBF server.



See the docs for ubf_server:start_link() for a description of the
`Options` proplist.<a name="install_default_handler-1"></a>

###install_default_handler/1##




<pre>install_default_handler(Pid::pid()) -&gt; ack</pre>
<br></br>






Install a default handler function (callback-style) for  
asynchronous UBF messages.

The default handler function, drop_fun/1, does nothing.<a name="install_handler-2"></a>

###install_handler/2##




<pre>install_handler(Pid::pid(), Fun::function()) -&gt; ack</pre>
<br></br>






Install a handler function (callback-style) for asynchronous  
UBF messages.



The handler fun Fun should be a function of arity 1.  When an  
asynchronous UBF message is received, the callback function will be  
called with the UBF message as its single argument.  The Fun is  
called by the ubf client process so the Fun can crash and/or block  
this process.

If your handler fun must maintain its own state, then you must use
an intermediate anonymous fun to bind the state.  See the usage of
the `irc_client_gs:send_self/2` fun as an example.  The
`send_self()` fun is actually arity 2, but the extra
argument is how the author, Joe Armstrong, maintains the extra
state required to deliver the async UBF message to the process that
is executing the event loop processing function,
`irc_client_gs:loop/6`.<a name="lpc-2"></a>

###lpc/2##




<pre>lpc(Mod::module(), Q::term()) -&gt; term()</pre>
<br></br>




Perform a synchronous LPC (local procedure) call with the
state 'none'.
<a name="lpc-3"></a>

###lpc/3##




<pre>lpc(Mod::module(), Q::term(), State::atom()) -&gt; term()</pre>
<br></br>




Perform a synchronous LPC (local procedure) call with the
specified state.
<a name="lpc-4"></a>

###lpc/4##




`lpc(Mod, Q, State, TLogMod) -> any()`

<a name="rpc-2"></a>

###rpc/2##




<pre>rpc(Pid::pid(), Q::term()) -&gt; timeout | term()</pre>
<br></br>






Perform a synchronous RPC call.

NOTE: It is not recommended that a UBF client return the bare atom
'timeout' in response to any RPC call.<a name="rpc-3"></a>

###rpc/3##




<pre>rpc(Pid::pid(), Q::term(), Timeout::timeout()) -&gt; timeout | term() | [exit](#type-exit)(badpid) | [exit](#type-exit)(badarg)</pre>
<br></br>




Perform a synchronous RPC call.<a name="sendEvent-2"></a>

###sendEvent/2##




<pre>sendEvent(Pid::pid(), Msg) -&gt; any()</pre>
<br></br>




Send an asynchronous UBF message.<a name="stop-1"></a>

###stop/1##




<pre>stop(Pid::pid()) -&gt; ok</pre>
<br></br>




Stop a UBF client process.
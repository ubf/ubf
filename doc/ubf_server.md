

#Module ubf_server#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


UBF server-side public API.

<a name="description"></a>

##Description##




TO-DO: JoeNorton, ubf_server:start([file_plugin], 2000, [jsf])   
will give a JSON-speaking server that has a lot of difficulty   
decoding and encoding stuff.  Do you have any plan to fix it?



This module implements most of the commonly-used server-side   
functions: starting TCP listeners and registering their   
implementation callback modules.



We implement three different wire formats for accessing the same   
implementation of a UBF(B) protocol-checking server:


* UBF(A).  This is Joe Armstrong's original implementation.
See [
http://www.sics.se/~joe/ubf/](http://www.sics.se/~joe/ubf/) for details.

* EBF, a.k.a. Erlang Binary Format.  This protocol uses common
Erlang wire formats, the `{packet, 4}` protocol from
'inets' for TCP connections, and the
`term_to_binary()`/`binary_to_term()` BIFs for
payload encoding.  These wire formats are used to pass Erlang
terms between a UBF(B) contract checking server and a client
that does not support the UBF(A) wire format but does support
Erlang's native wire formats.

* JSF, a.k.a the JSon Format.  Similar to EBF, except
that JavaScript's JSON encoding is used for the wire protocol
instead of UBF(A) or Erlang's native wire formats.

* TBF, a.k.a the Thrift Binary Format.  Similar to EBF, except
that Thrift's binary encoding is used for the wire protocol
instead of UBF(A) or Erlang's native wire formats.

* PBF, a.k.a the Google's Protocol Buffers Format.  Similar to
EBF, except that Google's Protocol Buffers binary encoding is used
for the wire protocol instead of UBF(A) or Erlang's native wire
formats.

* ABF, a.k.a the Avro Binary Format.  Similar to EBF, except
that Avro's binary encoding is used for the wire protocol
instead of UBF(A) or Erlang's native wire formats.





There is no "stop" function.  To stop the server, instead stop the
TCP service manager that controls it: see the `proc_socket_server`   
module for extra details.

See the documentation for the `file_plugin` module for
extra commentary on writing an UBF server implementation module.
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-5">init/5</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Start a server and a TCP listener on port Port and register  
all of the protocol implementation modules in the Plugins list.</td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td>Start a registered server and a TCP listener on port Port and  
register all of the protocol implementation modules in the Plugins  
list.</td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td>Start a registered server and a TCP listener on port Port with  
the Options properties list and register all of the protocol  
implementation modules in the Plugins list.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>See start/2, but also link the server processs to the caller.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>See start/3, but also link the server processs to the caller.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td>See start/4, but also link the server processs to the caller.</td></tr><tr><td valign="top"><a href="#start_term_listener-3">start_term_listener/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="init-5"></a>

###init/5##




`init(Name, Parent, Plugins, Port, Options) -> any()`

<a name="start-2"></a>

###start/2##




<pre>start(Plugins::[atom()], Port::integer()) -&gt; true</pre>
<br></br>






Start a server and a TCP listener on port Port and register  
all of the protocol implementation modules in the Plugins list.

Here we start the server.<a name="start-3"></a>

###start/3##




<pre>start(Name::atom(), Plugins::[atom()], Port::integer()) -&gt; true</pre>
<br></br>






Start a registered server and a TCP listener on port Port and  
register all of the protocol implementation modules in the Plugins  
list. If Name is undefined, the server is not registered.

Here we start the server.<a name="start-4"></a>

###start/4##




<pre>start(Name::atom(), Plugins::[atom()], Port::integer(), Options::[proplist()](#type-proplist)) -&gt; true</pre>
<br></br>






Start a registered server and a TCP listener on port Port with  
the Options properties list and register all of the protocol  
implementation modules in the Plugins list.  If Name is undefined,  
the server is not registered

Valid properties in the Options proplist are:

* {idletimer, integer() | infinity} ... Maximum time (in milliseconds)
that a client connection may remain idle before the server will
close the connection.
Default: infinity

* {maxconn, integer()} ... Maximum number of simultaneous TCP
connections allowed.
Default: 10,000.

* {proto, {ubf | ebf | atom()}} ... Enable the UBF, EBF, or
an alternative protocol wire format.
Default: ubf.

* {proto, {ubf | ebf | atom(), proplist()}} ... Enable the UBF,
EBF, or an alternative protocol wire format with options.
Default: {ubf, []}.


Supported options:

* safe ... Prevents decoding data that may be used to
attack the Erlang system.  In the event of receiving unsafe
data, decoding fails with a badarg error.



* {registeredname, atom()} ... Set the name to be registered for
the TCP listener.  If undefined, a default name is automatically
registered.
Default: undefined.

* {statelessrpc, true | false} ... Run the stateless variety of
a UBF(B) contract.  A stateless contract is an extension of
Joe Armstrong's original UBF server implementation.
Default: false.

* {startplugin, atom()} ... Set the starting plugin, set after a
client first connects to the server.  If not set, client may
select the service using the startSession() API.  There is
no default setting.

* {serverhello, string() | undefined} ... Meta contract greeting
string, sent when a client first connects to the server.  If
undefined, server hello is not sent to the client.
Default: "meta_server".

* {simplerpc, true | false} ... Set the simple RPC mode.  If
true, server returns only the rpc reply to client.  If false,
server returns the rpc reply and next state to client.
Default: false.

* {verboserpc, true | false} ... Set the verbose RPC mode.  If
true, server calls the plugin handler with the rpc request and
matched contract types.  If false, server calls the plugin
handler only with the rpc request.
Default: false.

* {tlog_module, atom() | {atom(), boolean()}} ... Set the transaction
log callback module and optionally control the built-in calls
by 'contract_manager_tlog' to the 'error_logger' module.
If the 2-tuple representation is used and the boolean() member is
false, then calls to 'error_logger' will not be attempted.
Default: undefined.

* {process_options, list()} ... Specify additional options used
for spawning server and/or client related erlang processes.
Typically used to specify non-default, garbage collection options.
Default: [].


<a name="start_link-2"></a>

###start_link/2##




<pre>start_link(Plugins::[atom()], Port::integer()) -&gt; true</pre>
<br></br>




See start/2, but also link the server processs to the caller.<a name="start_link-3"></a>

###start_link/3##




<pre>start_link(Name::atom(), Plugins::[atom()], Port::integer()) -&gt; true</pre>
<br></br>




See start/3, but also link the server processs to the caller.<a name="start_link-4"></a>

###start_link/4##




<pre>start_link(Name::atom(), Plugins::[atom()], Port::integer(), Options::[proplist()](#type-proplist)) -&gt; true</pre>
<br></br>




See start/4, but also link the server processs to the caller.<a name="start_term_listener-3"></a>

###start_term_listener/3##




`start_term_listener(Server0, Plugins, Options) -> any()`


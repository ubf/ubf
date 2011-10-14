

#Module ubf_server#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


<p>UBF server-side public API.</p>


<pre><tt>This module implements most of the commonly-used server-side
functions: starting TCP listeners and registering their
implementation callback modules.</tt></pre>



<pre><tt>We implement several different wire formats for accessing the same
implementation of a UBF(b) protocol-checking server:</tt></pre>

<ul>
<li>
<p>
UBF(a).  This is Joe Armstrong's original implementation.
</p>
</li>
<li>
<p>
EBF, a.k.a. Erlang Binary Format.  This protocol uses common
Erlang wire formats, the <tt>{packet, 4}</tt> protocol from <tt>inets</tt> for
TCP connections, and the <tt>term_to_binary()</tt>/<tt>binary_to_term()</tt>
BIFs for payload encoding.  These wire formats are used to pass
Erlang terms between a UBF(b) contract checking server and a
client that does not support the UBF(a) wire format but does
support Erlang's native wire formats.
</p>
</li>
<li>
<p>
JSF, a.k.a the JSon Format.  Similar to EBF, except that
JavaScript's JSON encoding is used for the wire protocol
instead of UBF(a) or Erlang's native wire formats.
</p>
</li>
<li>
<p>
TBF, a.k.a the Thrift Binary Format.  Similar to EBF, except
that Thrift's binary encoding is used for the wire protocol
instead of UBF(a) or Erlang's native wire formats.
</p>


<pre><tt>There is no "stop" function.  To stop the server, instead stop the
TCP service manager that controls it: see the +proc_socket_server+
module for extra details.</tt></pre>



<pre><tt>See the documentation for the +file_plugin+ module for extra
commentary on writing an UBF server implementation module.</tt></pre>

</li>
</ul>.


<a name="types"></a>

##Data Types##




###<a name="type-ipport">ipport()</a>##



<pre>ipport() = pos_integer()</pre>



###<a name="type-name">name()</a>##



<pre>name() = atom()</pre>



###<a name="type-options">options()</a>##



<pre>options() = [{atom(), term()}]</pre>



###<a name="type-plugins">plugins()</a>##



<pre>plugins() = [module()]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-5">init/5</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td><p>Start a server and a TCP listener on port Port and register
all of the protocol implementation modules in the Plugins list.</p>


<pre><tt>Here we start the server.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td><p>Start a registered server and a TCP listener on port Port and
register all of the protocol implementation modules in the Plugins
list. If Name is undefined, the server is not registered.</p>


<pre><tt>Here we start the server.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td><p>Start a registered server and a TCP listener on port Port with
the Options properties list and register all of the protocol
implementation modules in the Plugins list.  If Name is undefined,
the server is not registered</p>


<pre><tt>Valid properties in the Options proplist are:</tt></pre>

<ul>
<li>
<p>
<tt>{idletimer, integer() | infinity}</tt> Maximum time (in milliseconds)
that a client connection may remain idle before the server will
close the connection.
Default: infinity
</p>
</li>
<li>
<p>
<tt>{maxconn, integer()}</tt> Maximum number of simultaneous TCP
connections allowed.
Default: 10,000.
</p>
</li>
<li>
<p>
<tt>{proto, {ubf | ebf | atom()}}</tt> Enable the UBF, EBF, or
an alternative protocol wire format.
Default: ubf.
</p>
</li>
<li>
<p>
<tt>{proto, {ubf | ebf | atom(), proplist()}}</tt> Enable the UBF,
EBF, or an alternative protocol wire format with options.
Default: <tt>{ubf, []}</tt>.
</p>


<pre><tt>Supported options:
- safe  Prevents decoding data that may be used to attack the
  Erlang system.  In the event of receiving unsafe data, decoding
  fails with a badarg error.</tt></pre>

</li>
<li>
<p>
<tt>{registeredname, atom()}</tt> Set the name to be registered for
the TCP listener.  If undefined, a default name is automatically
registered.
Default: undefined.
</p>
</li>
<li>
<p>
<tt>{statelessrpc, true | false}</tt> Run the stateless variety of
a UBF(b) contract.  A stateless contract is an extension of
Joe Armstrong's original UBF server implementation.
Default: false.
</p>
</li>
<li>
<p>
<tt>{startplugin, atom()}</tt> Set the starting plugin, set after a
client first connects to the server.  If not set, client may
select the service using the startSession() API.  There is
no default setting.
</p>
</li>
<li>
<p>
<tt>{serverhello, string() | undefined}</tt> Meta contract greeting
string, sent when a client first connects to the server.  If
undefined, server hello is not sent to the client.
Default: "meta_server".
</p>
</li>
<li>
<p>
<tt>{simplerpc, true | false}</tt> Set the simple RPC mode.  If
true, server returns only the rpc reply to client.  If false,
server returns the rpc reply and next state to client.
Default: false.
</p>
</li>
<li>
<p>
<tt>{verboserpc, true | false}</tt> Set the verbose RPC mode.  If
true, server calls the plugin handler with the rpc request and
matched contract types.  If false, server calls the plugin
handler only with the rpc request.
Default: false.
</p>
</li>
<li>
<p>
<tt>{tlog_module, atom() | {atom(), boolean()}}</tt> Set the transaction
log callback module and optionally control the built-in calls
by <tt>contract_manager_tlog</tt> to the <tt>error_logger</tt> module.
If the 2-tuple representation is used and the boolean() member is
false, then calls to <tt>error_logger</tt> will not be attempted.
Default: undefined.
</p>
</li>
<li>
<p>
<tt>{process_options, list()}</tt> Specify additional options used
for spawning server and/or client related erlang processes.
Typically used to specify non-default, garbage collection options.
Default: [].
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td><p>See start/2, but also link the server processs to the caller.</p>.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td><p>See start/3, but also link the server processs to the caller.</p>.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td><p>See start/4, but also link the server processs to the caller.</p>.</td></tr><tr><td valign="top"><a href="#start_term_listener-3">start_term_listener/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="init-5"></a>

###init/5##




<pre>init(Name::[name()](#type-name), Parent::pid(), Plugins::[plugins()](#type-plugins), Port::[ipport()](#type-ipport), Options::[options()](#type-options)) -&gt; pid()</pre>
<br></br>


<a name="start-2"></a>

###start/2##




<pre>start(Plugins::[plugins()](#type-plugins), Port::[ipport()](#type-ipport)) -&gt; true</pre>
<br></br>




<p>Start a server and a TCP listener on port Port and register
all of the protocol implementation modules in the Plugins list.</p>


<pre><tt>Here we start the server.</tt></pre>

<a name="start-3"></a>

###start/3##




<pre>start(Name::[name()](#type-name), Plugins::[plugins()](#type-plugins), Port::[ipport()](#type-ipport)) -&gt; true</pre>
<br></br>




<p>Start a registered server and a TCP listener on port Port and
register all of the protocol implementation modules in the Plugins
list. If Name is undefined, the server is not registered.</p>


<pre><tt>Here we start the server.</tt></pre>

<a name="start-4"></a>

###start/4##




<pre>start(Name::[name()](#type-name), Plugins::[plugins()](#type-plugins), Port::[ipport()](#type-ipport), Options::[options()](#type-options)) -&gt; true</pre>
<br></br>




<p>Start a registered server and a TCP listener on port Port with
the Options properties list and register all of the protocol
implementation modules in the Plugins list.  If Name is undefined,
the server is not registered</p>


<pre><tt>Valid properties in the Options proplist are:</tt></pre>

<ul>
<li>
<p>
<tt>{idletimer, integer() | infinity}</tt> Maximum time (in milliseconds)
that a client connection may remain idle before the server will
close the connection.
Default: infinity
</p>
</li>
<li>
<p>
<tt>{maxconn, integer()}</tt> Maximum number of simultaneous TCP
connections allowed.
Default: 10,000.
</p>
</li>
<li>
<p>
<tt>{proto, {ubf | ebf | atom()}}</tt> Enable the UBF, EBF, or
an alternative protocol wire format.
Default: ubf.
</p>
</li>
<li>
<p>
<tt>{proto, {ubf | ebf | atom(), proplist()}}</tt> Enable the UBF,
EBF, or an alternative protocol wire format with options.
Default: <tt>{ubf, []}</tt>.
</p>


<pre><tt>Supported options:
- safe  Prevents decoding data that may be used to attack the
  Erlang system.  In the event of receiving unsafe data, decoding
  fails with a badarg error.</tt></pre>

</li>
<li>
<p>
<tt>{registeredname, atom()}</tt> Set the name to be registered for
the TCP listener.  If undefined, a default name is automatically
registered.
Default: undefined.
</p>
</li>
<li>
<p>
<tt>{statelessrpc, true | false}</tt> Run the stateless variety of
a UBF(b) contract.  A stateless contract is an extension of
Joe Armstrong's original UBF server implementation.
Default: false.
</p>
</li>
<li>
<p>
<tt>{startplugin, atom()}</tt> Set the starting plugin, set after a
client first connects to the server.  If not set, client may
select the service using the startSession() API.  There is
no default setting.
</p>
</li>
<li>
<p>
<tt>{serverhello, string() | undefined}</tt> Meta contract greeting
string, sent when a client first connects to the server.  If
undefined, server hello is not sent to the client.
Default: "meta_server".
</p>
</li>
<li>
<p>
<tt>{simplerpc, true | false}</tt> Set the simple RPC mode.  If
true, server returns only the rpc reply to client.  If false,
server returns the rpc reply and next state to client.
Default: false.
</p>
</li>
<li>
<p>
<tt>{verboserpc, true | false}</tt> Set the verbose RPC mode.  If
true, server calls the plugin handler with the rpc request and
matched contract types.  If false, server calls the plugin
handler only with the rpc request.
Default: false.
</p>
</li>
<li>
<p>
<tt>{tlog_module, atom() | {atom(), boolean()}}</tt> Set the transaction
log callback module and optionally control the built-in calls
by <tt>contract_manager_tlog</tt> to the <tt>error_logger</tt> module.
If the 2-tuple representation is used and the boolean() member is
false, then calls to <tt>error_logger</tt> will not be attempted.
Default: undefined.
</p>
</li>
<li>
<p>
<tt>{process_options, list()}</tt> Specify additional options used
for spawning server and/or client related erlang processes.
Typically used to specify non-default, garbage collection options.
Default: [].
</p>
</li>
</ul>
<a name="start_link-2"></a>

###start_link/2##




<pre>start_link(Plugins::[plugins()](#type-plugins), Port::[ipport()](#type-ipport)) -&gt; true</pre>
<br></br>




<p>See start/2, but also link the server processs to the caller.</p>
<a name="start_link-3"></a>

###start_link/3##




<pre>start_link(Name::[name()](#type-name), Plugins::[plugins()](#type-plugins), Port::[ipport()](#type-ipport)) -&gt; true</pre>
<br></br>




<p>See start/3, but also link the server processs to the caller.</p>
<a name="start_link-4"></a>

###start_link/4##




<pre>start_link(Name::[name()](#type-name), Plugins::[plugins()](#type-plugins), Port::[ipport()](#type-ipport), Options::[options()](#type-options)) -&gt; true</pre>
<br></br>




<p>See start/4, but also link the server processs to the caller.</p>
<a name="start_term_listener-3"></a>

###start_term_listener/3##




<pre>start_term_listener(Server0::pid(), Plugins::[plugins()](#type-plugins), Options::[options()](#type-options)) -&gt; pid()</pre>
<br></br>



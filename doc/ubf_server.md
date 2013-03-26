

# Module ubf_server #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


<p>UBF server-side public API.</p>


<pre><code>This module implements most of the commonly-used server-side
functions: starting TCP listeners and registering their
implementation callback modules.</code></pre>



<pre><code>We implement several different wire formats for accessing the same
implementation of a UBF(b) protocol-checking server:</code></pre>

<ul>
<li>
<p>
UBF(a).  This is Joe Armstrong's original implementation.
</p>
</li>
<li>
<p>
EBF, a.k.a. Erlang Binary Format.  This protocol uses common
Erlang wire formats, the <code>{packet, 4}</code> protocol from <code>inets</code> for
TCP connections, and the <code>term_to_binary()</code>/<code>binary_to_term()</code>
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


<pre><code>There is no "stop" function.  To stop the server, instead stop the
TCP service manager that controls it: see the +proc_socket_server+
module for extra details.</code></pre>



<pre><code>See the documentation for the +file_plugin+ module for extra
commentary on writing an UBF server implementation module.</code></pre>

</li>
</ul>.


<a name="types"></a>

## Data Types ##




### <a name="type-ipport">ipport()</a> ###



<pre><code>
ipport() = pos_integer()
</code></pre>





### <a name="type-name">name()</a> ###



<pre><code>
name() = atom()
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [{atom(), term()}]
</code></pre>





### <a name="type-plugins">plugins()</a> ###



<pre><code>
plugins() = [module()]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-5">init/5</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td><p>Start a server and a TCP listener on port Port and register
all of the protocol implementation modules in the Plugins list.</p>


<pre><code>Here we start the server.</code></pre>
.</td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td><p>Start a registered server and a TCP listener on port Port and
register all of the protocol implementation modules in the Plugins
list. If Name is undefined, the server is not registered.</p>


<pre><code>Here we start the server.</code></pre>
.</td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td><p>Start a registered server and a TCP listener on port Port with
the Options properties list and register all of the protocol
implementation modules in the Plugins list.  If Name is undefined,
the server is not registered</p>


<pre><code>Valid properties in the Options proplist are:</code></pre>

<ul>
<li>
<p>
<code>{idletimer, integer() | infinity}</code> Maximum time (in milliseconds)
that a client connection may remain idle before the server will
close the connection.
Default: infinity
</p>
</li>
<li>
<p>
<code>{maxconn, integer()}</code> Maximum number of simultaneous TCP
connections allowed.
Default: 10,000.
</p>
</li>
<li>
<p>
<code>{proto, {ubf | ebf | atom()}}</code> Enable the UBF, EBF, or
an alternative protocol wire format.
Default: ubf.
</p>
</li>
<li>
<p>
<code>{proto, {ubf | ebf | atom(), [atom() | tuple()]}}</code> Enable the UBF,
EBF, or an alternative protocol wire format with options.
Default: <code>{ubf, []}</code>.
</p>


<pre><code>Supported options:
- safe  Prevents decoding data that may be used to attack the
  Erlang system.  In the event of receiving unsafe data, decoding
  fails with a badarg error.</code></pre>

</li>
<li>
<p>
<code>{registeredname, atom()}</code> Set the name to be registered for
the TCP listener.  If undefined, a default name is automatically
registered.
Default: undefined.
</p>
</li>
<li>
<p>
<code>{statelessrpc, true | false}</code> Run the stateless variety of
a UBF(b) contract.  A stateless contract is an extension of
Joe Armstrong's original UBF server implementation.
Default: false.
</p>
</li>
<li>
<p>
<code>{startplugin, atom()}</code> Set the starting plugin, set after a
client first connects to the server.  If not set, client may
select the service using the startSession() API.  There is
no default setting.
</p>
</li>
<li>
<p>
<code>{serverhello, ubfstring() | undefined}</code> Meta contract greeting
string, sent when a client first connects to the server.  If
undefined, server hello is not sent to the client.
Default: "meta_server".
</p>
</li>
<li>
<p>
<code>{simplerpc, true | false}</code> Set the simple RPC mode.  If
true, server returns only the rpc reply to client.  If false,
server returns the rpc reply and next state to client.
Default: false.
</p>
</li>
<li>
<p>
<code>{verboserpc, true | false}</code> Set the verbose RPC mode.  If
true, server calls the plugin handler with the rpc request and
matched contract types.  If false, server calls the plugin
handler only with the rpc request.
Default: false.
</p>
</li>
<li>
<p>
<code>{tlog_module, atom() | {atom(), boolean()}}</code> Set the transaction
log callback module and optionally control the built-in calls
by <code>contract_manager_tlog</code> to the <code>error_logger</code> module.
If the 2-tuple representation is used and the boolean() member is
false, then calls to <code>error_logger</code> will not be attempted.
Default: undefined.
</p>
</li>
<li>
<p>
<code>{process_options, list()}</code> Specify additional options used
for spawning server and/or client related erlang processes.
Typically used to specify non-default, garbage collection options.
Default: [].
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td><p>See start/2, but also link the server processs to the caller.</p>.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td><p>See start/3, but also link the server processs to the caller.</p>.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td><p>See start/4, but also link the server processs to the caller.</p>.</td></tr><tr><td valign="top"><a href="#start_term_listener-3">start_term_listener/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-5"></a>

### init/5 ###


<pre><code>
init(Name::<a href="#type-name">name()</a>, Parent::pid(), Plugins::<a href="#type-plugins">plugins()</a>, Port::<a href="#type-ipport">ipport()</a>, Options::<a href="#type-options">options()</a>) -&gt; pid()
</code></pre>

<br></br>



<a name="start-2"></a>

### start/2 ###


<pre><code>
start(Plugins::<a href="#type-plugins">plugins()</a>, Port::<a href="#type-ipport">ipport()</a>) -&gt; true
</code></pre>

<br></br>


<p>Start a server and a TCP listener on port Port and register
all of the protocol implementation modules in the Plugins list.</p>


<pre><code>Here we start the server.</code></pre>


<a name="start-3"></a>

### start/3 ###


<pre><code>
start(Name::<a href="#type-name">name()</a>, Plugins::<a href="#type-plugins">plugins()</a>, Port::<a href="#type-ipport">ipport()</a>) -&gt; true
</code></pre>

<br></br>


<p>Start a registered server and a TCP listener on port Port and
register all of the protocol implementation modules in the Plugins
list. If Name is undefined, the server is not registered.</p>


<pre><code>Here we start the server.</code></pre>


<a name="start-4"></a>

### start/4 ###


<pre><code>
start(Name::<a href="#type-name">name()</a>, Plugins::<a href="#type-plugins">plugins()</a>, Port::<a href="#type-ipport">ipport()</a>, Options::<a href="#type-options">options()</a>) -&gt; true
</code></pre>

<br></br>


<p>Start a registered server and a TCP listener on port Port with
the Options properties list and register all of the protocol
implementation modules in the Plugins list.  If Name is undefined,
the server is not registered</p>


<pre><code>Valid properties in the Options proplist are:</code></pre>

<ul>
<li>
<p>
<code>{idletimer, integer() | infinity}</code> Maximum time (in milliseconds)
that a client connection may remain idle before the server will
close the connection.
Default: infinity
</p>
</li>
<li>
<p>
<code>{maxconn, integer()}</code> Maximum number of simultaneous TCP
connections allowed.
Default: 10,000.
</p>
</li>
<li>
<p>
<code>{proto, {ubf | ebf | atom()}}</code> Enable the UBF, EBF, or
an alternative protocol wire format.
Default: ubf.
</p>
</li>
<li>
<p>
<code>{proto, {ubf | ebf | atom(), [atom() | tuple()]}}</code> Enable the UBF,
EBF, or an alternative protocol wire format with options.
Default: <code>{ubf, []}</code>.
</p>


<pre><code>Supported options:
- safe  Prevents decoding data that may be used to attack the
  Erlang system.  In the event of receiving unsafe data, decoding
  fails with a badarg error.</code></pre>

</li>
<li>
<p>
<code>{registeredname, atom()}</code> Set the name to be registered for
the TCP listener.  If undefined, a default name is automatically
registered.
Default: undefined.
</p>
</li>
<li>
<p>
<code>{statelessrpc, true | false}</code> Run the stateless variety of
a UBF(b) contract.  A stateless contract is an extension of
Joe Armstrong's original UBF server implementation.
Default: false.
</p>
</li>
<li>
<p>
<code>{startplugin, atom()}</code> Set the starting plugin, set after a
client first connects to the server.  If not set, client may
select the service using the startSession() API.  There is
no default setting.
</p>
</li>
<li>
<p>
<code>{serverhello, ubfstring() | undefined}</code> Meta contract greeting
string, sent when a client first connects to the server.  If
undefined, server hello is not sent to the client.
Default: "meta_server".
</p>
</li>
<li>
<p>
<code>{simplerpc, true | false}</code> Set the simple RPC mode.  If
true, server returns only the rpc reply to client.  If false,
server returns the rpc reply and next state to client.
Default: false.
</p>
</li>
<li>
<p>
<code>{verboserpc, true | false}</code> Set the verbose RPC mode.  If
true, server calls the plugin handler with the rpc request and
matched contract types.  If false, server calls the plugin
handler only with the rpc request.
Default: false.
</p>
</li>
<li>
<p>
<code>{tlog_module, atom() | {atom(), boolean()}}</code> Set the transaction
log callback module and optionally control the built-in calls
by <code>contract_manager_tlog</code> to the <code>error_logger</code> module.
If the 2-tuple representation is used and the boolean() member is
false, then calls to <code>error_logger</code> will not be attempted.
Default: undefined.
</p>
</li>
<li>
<p>
<code>{process_options, list()}</code> Specify additional options used
for spawning server and/or client related erlang processes.
Typically used to specify non-default, garbage collection options.
Default: [].
</p>
</li>
</ul>

<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(Plugins::<a href="#type-plugins">plugins()</a>, Port::<a href="#type-ipport">ipport()</a>) -&gt; true
</code></pre>

<br></br>


<p>See start/2, but also link the server processs to the caller.</p>

<a name="start_link-3"></a>

### start_link/3 ###


<pre><code>
start_link(Name::<a href="#type-name">name()</a>, Plugins::<a href="#type-plugins">plugins()</a>, Port::<a href="#type-ipport">ipport()</a>) -&gt; true
</code></pre>

<br></br>


<p>See start/3, but also link the server processs to the caller.</p>

<a name="start_link-4"></a>

### start_link/4 ###


<pre><code>
start_link(Name::<a href="#type-name">name()</a>, Plugins::<a href="#type-plugins">plugins()</a>, Port::<a href="#type-ipport">ipport()</a>, Options::<a href="#type-options">options()</a>) -&gt; true
</code></pre>

<br></br>


<p>See start/4, but also link the server processs to the caller.</p>

<a name="start_term_listener-3"></a>

### start_term_listener/3 ###


<pre><code>
start_term_listener(Server0::pid(), Plugins::<a href="#type-plugins">plugins()</a>, Options::<a href="#type-options">options()</a>) -&gt; pid()
</code></pre>

<br></br>




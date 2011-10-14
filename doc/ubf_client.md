

#Module ubf_client#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


<p>UBF client-side public API.</p>


<pre><tt>This module implements most of the commonly-used client-side
functions required to talk to UBF servers:</tt></pre>

<ul>
<li>
<p>
<tt>connect()</tt> to a UBF server
</p>
</li>
<li>
<p>
<tt>rpc()</tt> to make a synchronous call to a connected UBF server
</p>
</li>
<li>
<p>
<tt>stop()</tt> a connection
</p>
</li>
<li>
<p>
<tt>install_handler()</tt> to add a callback function to handle
asynchronous notifications from theUBF server to your client
process.
</p>


<pre><tt>Note that this library can support UBF(a), EBF, JSF, TBF, PBF, and
ABF transport.  See the +connect()+ function arguments for
details.</tt></pre>



<pre><tt>This module also provides an alternative client-side function for
calling\'s UBF contract manager and a UBF contract\'s
implementation without any side-effects: +lpc()+ to make a
synchronous local procedure call to a contract\'s implementation.</tt></pre>

</li>
</ul>.


<a name="types"></a>

##Data Types##




###<a name="type-host">host()</a>##



<pre>host() = nonempty_string()</pre>



###<a name="type-ipport">ipport()</a>##



<pre>ipport() = pos_integer()</pre>



###<a name="type-name">name()</a>##



<pre>name() = atom()</pre>



###<a name="type-options">options()</a>##



<pre>options() = [{atom(), term()}]</pre>



###<a name="type-plugin">plugin()</a>##



<pre>plugin() = module()</pre>



###<a name="type-plugins">plugins()</a>##



<pre>plugins() = [[plugin()](#type-plugin)]</pre>



###<a name="type-server">server()</a>##



<pre>server() = [name()](#type-name) | pid()</pre>



###<a name="type-service">service()</a>##



<pre>service() = {'#S', nonempty_string()} | undefined</pre>



###<a name="type-statename">statename()</a>##



<pre>statename() = atom()</pre>



###<a name="type-tlogger">tlogger()</a>##



<pre>tlogger() = module()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td><p>Connect to a UBF server at address Host + TCP port Port.</p>.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td><p>Connect to a UBF server at address Host + TCP port Port.</p>.</td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td><p>Connect to a UBF server at address Host + TCP port Port, or at
pid/registered name Server.</p>


<pre><tt>When using the alternate form, the first two arguments are:</tt></pre>

<ul>
<li>
<p>
Plugins: a <tt>plugin_module_list()</tt>.
</p>
</li>
<li>
<p>
Server: either a process id <tt>(pid())</tt> or process registered
name <tt>(atom())</tt> for an already-started UBF server.
</p>


<pre><tt>See the docs for +ubf_server:start_link()+ for a description of the
+Options+ proplist.</tt></pre>

</li>
</ul>.</td></tr><tr><td valign="top"><a href="#install_default_handler-1">install_default_handler/1</a></td><td><p>Install a default handler function (callback-style) for
asynchronous UBF messages.</p>


<pre><tt>The default handler function, drop_fun/1, does nothing.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#install_handler-2">install_handler/2</a></td><td><p>Install a handler function (callback-style) for asynchronous
UBF messages.</p>


<pre><tt>The handler fun Fun should be a function of arity 1.  When an
asynchronous UBF message is received, the callback function will be
called with the UBF message as its single argument.  The Fun is
called by the ubf client process so the Fun can crash and/or block
this process.</tt></pre>



<pre><tt>If your handler fun must maintain its own state, then you must use
an intermediate anonymous fun to bind the state.  See the usage of
the +irc_client_gs:send_self/2+ fun as an example.  The
+send_self()+ fun is actually arity 2, but the extra argument is
how the author, Joe Armstrong, maintains the extra state required
to deliver the async UBF message to the process that is executing
the event loop processing function, +irc_client_gs:loop/6+.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#lpc-2">lpc/2</a></td><td><p>Perform a synchronous LPC (local procedure) call with the
state <tt>none</tt>.</p>.</td></tr><tr><td valign="top"><a href="#lpc-3">lpc/3</a></td><td><p>Perform a synchronous LPC (local procedure) call with the
specified state.</p>.</td></tr><tr><td valign="top"><a href="#lpc-4">lpc/4</a></td><td></td></tr><tr><td valign="top"><a href="#rpc-2">rpc/2</a></td><td><p>Perform a synchronous RPC call.</p>


<pre><tt>NOTE: It is not recommended that a UBF client return the bare atom
+timeout+ in response to any RPC call.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#rpc-3">rpc/3</a></td><td><p>Perform a synchronous RPC call.</p>.</td></tr><tr><td valign="top"><a href="#sendEvent-2">sendEvent/2</a></td><td><p>Send an asynchronous UBF message.</p>.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td><p>Stop a UBF client process.</p>.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="connect-2"></a>

###connect/2##




<pre>connect(Host::[host()](#type-host) | [plugins()](#type-plugins), Port::[ipport()](#type-ipport) | [server()](#type-server)) -&gt; {ok, Client::pid(), [service()](#type-service)} | {error, term()}</pre>
<br></br>




<p>Connect to a UBF server at address Host + TCP port Port.</p>
<a name="connect-3"></a>

###connect/3##




<pre>connect(Host::[host()](#type-host) | [plugins()](#type-plugins), Port::[ipport()](#type-ipport) | [server()](#type-server), Timeout::timeout()) -&gt; {ok, Client::pid(), [service()](#type-service)} | {error, term()}</pre>
<br></br>




<p>Connect to a UBF server at address Host + TCP port Port.</p>
<a name="connect-4"></a>

###connect/4##




<pre>connect(X::[host()](#type-host) | [plugins()](#type-plugins), Y::[ipport()](#type-ipport) | [server()](#type-server), Options::[options()](#type-options), Timeout::timeout()) -&gt; {ok, Client::pid(), [service()](#type-service)} | {error, term()}</pre>
<br></br>




<p>Connect to a UBF server at address Host + TCP port Port, or at
pid/registered name Server.</p>


<pre><tt>When using the alternate form, the first two arguments are:</tt></pre>

<ul>
<li>
<p>
Plugins: a <tt>plugin_module_list()</tt>.
</p>
</li>
<li>
<p>
Server: either a process id <tt>(pid())</tt> or process registered
name <tt>(atom())</tt> for an already-started UBF server.
</p>


<pre><tt>See the docs for +ubf_server:start_link()+ for a description of the
+Options+ proplist.</tt></pre>

</li>
</ul>
<a name="install_default_handler-1"></a>

###install_default_handler/1##




<pre>install_default_handler(Client::pid()) -&gt; ack</pre>
<br></br>




<p>Install a default handler function (callback-style) for
asynchronous UBF messages.</p>


<pre><tt>The default handler function, drop_fun/1, does nothing.</tt></pre>

<a name="install_handler-2"></a>

###install_handler/2##




<pre>install_handler(Client::pid(), Fun::function()) -&gt; ack</pre>
<br></br>




<p>Install a handler function (callback-style) for asynchronous
UBF messages.</p>


<pre><tt>The handler fun Fun should be a function of arity 1.  When an
asynchronous UBF message is received, the callback function will be
called with the UBF message as its single argument.  The Fun is
called by the ubf client process so the Fun can crash and/or block
this process.</tt></pre>



<pre><tt>If your handler fun must maintain its own state, then you must use
an intermediate anonymous fun to bind the state.  See the usage of
the +irc_client_gs:send_self/2+ fun as an example.  The
+send_self()+ fun is actually arity 2, but the extra argument is
how the author, Joe Armstrong, maintains the extra state required
to deliver the async UBF message to the process that is executing
the event loop processing function, +irc_client_gs:loop/6+.</tt></pre>

<a name="lpc-2"></a>

###lpc/2##




<pre>lpc(Mod::[plugin()](#type-plugin), Call::term()) -&gt; term()</pre>
<br></br>




<p>Perform a synchronous LPC (local procedure) call with the
state <tt>none</tt>.</p>
<a name="lpc-3"></a>

###lpc/3##




<pre>lpc(Mod::[plugin()](#type-plugin), Call::term(), State::[statename()](#type-statename)) -&gt; term()</pre>
<br></br>




<p>Perform a synchronous LPC (local procedure) call with the
specified state.</p>
<a name="lpc-4"></a>

###lpc/4##




<pre>lpc(Mod::[plugin()](#type-plugin), Call::term(), State::[statename()](#type-statename), TLogMod::[tlogger()](#type-tlogger)) -&gt; term()</pre>
<br></br>


<a name="rpc-2"></a>

###rpc/2##




<pre>rpc(Client::pid(), Call::term()) -&gt; timeout | term() | no_return()</pre>
<br></br>




<p>Perform a synchronous RPC call.</p>


<pre><tt>NOTE: It is not recommended that a UBF client return the bare atom
+timeout+ in response to any RPC call.</tt></pre>

<a name="rpc-3"></a>

###rpc/3##




<pre>rpc(Client::pid(), Call::term(), Timeout::timeout()) -&gt; timeout | term() | no_return()</pre>
<br></br>




<p>Perform a synchronous RPC call.</p>
<a name="sendEvent-2"></a>

###sendEvent/2##




<pre>sendEvent(Handler::pid(), Cast::term()) -&gt; ok | no_return()</pre>
<br></br>




<p>Send an asynchronous UBF message.</p>
<a name="stop-1"></a>

###stop/1##




<pre>stop(Client::pid()) -&gt; ok</pre>
<br></br>




<p>Stop a UBF client process.</p>

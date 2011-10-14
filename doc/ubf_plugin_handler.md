

#Module ubf_plugin_handler#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Implement the plugin server, an intermediate process between
the contract manager process and the server application.</p>


<pre><tt>The server application may or may not have a separate process (see
the diagram below).  The there is no application process(es), then
the remote procedure call will be executed by the process
executing this module\'s +loop()+ function.</tt></pre>



<pre><tt>This module also implements the plugin manager loop.</tt></pre>
.

<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ask_manager-2">ask_manager/2</a></td><td></td></tr><tr><td valign="top"><a href="#install_default_handler-1">install_default_handler/1</a></td><td><p>Install a default handler function (callback-style) for
asynchronous UBF messages.</p>


<pre><tt>The default handler function, drop_fun/1, does nothing.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#install_handler-2">install_handler/2</a></td><td><p>Install a handler function (callback-style) for asynchronous
UBF messages.</p>


<pre><tt>The handler fun Fun should be a function of arity 1.  When an
asynchronous UBF message is received, the callback function will be
called with the UBF message as its single argument.  The Fun is
called by the ubf plugin handler process so the Fun can crash
and/or block this process.</tt></pre>



<pre><tt>If your handler fun must maintain its own state, then you must use
an intermediate anonymous fun to bind the state.  See the usage of
the +irc_client_gs:send_self/2+ fun as an example.  The
+send_self()+ fun is actually arity 2, but the extra argument is
how the author, Joe Armstrong, maintains the extra state required
to deliver the async UBF message to the process that is executing
the event loop processing function, +irc_client_gs:loop/6+.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#manager-3">manager/3</a></td><td></td></tr><tr><td valign="top"><a href="#sendEvent-2">sendEvent/2</a></td><td><p>Send an asynchronous UBF message.</p>.</td></tr><tr><td valign="top"><a href="#start_handler-5">start_handler/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_manager-2">start_manager/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="ask_manager-2"></a>

###ask_manager/2##




<pre>ask_manager(Manager::pid(), Call::term()) -&gt; Reply::term()</pre>
<br></br>


<a name="install_default_handler-1"></a>

###install_default_handler/1##




<pre>install_default_handler(Handler::pid()) -&gt; ack</pre>
<br></br>




<p>Install a default handler function (callback-style) for
asynchronous UBF messages.</p>


<pre><tt>The default handler function, drop_fun/1, does nothing.</tt></pre>

<a name="install_handler-2"></a>

###install_handler/2##




<pre>install_handler(Handler::pid(), Fun::function()) -&gt; ack</pre>
<br></br>




<p>Install a handler function (callback-style) for asynchronous
UBF messages.</p>


<pre><tt>The handler fun Fun should be a function of arity 1.  When an
asynchronous UBF message is received, the callback function will be
called with the UBF message as its single argument.  The Fun is
called by the ubf plugin handler process so the Fun can crash
and/or block this process.</tt></pre>



<pre><tt>If your handler fun must maintain its own state, then you must use
an intermediate anonymous fun to bind the state.  See the usage of
the +irc_client_gs:send_self/2+ fun as an example.  The
+send_self()+ fun is actually arity 2, but the extra argument is
how the author, Joe Armstrong, maintains the extra state required
to deliver the async UBF message to the process that is executing
the event loop processing function, +irc_client_gs:loop/6+.</tt></pre>

<a name="manager-3"></a>

###manager/3##




`manager(ExitPid, Mod, Args) -> any()`

<a name="sendEvent-2"></a>

###sendEvent/2##




<pre>sendEvent(Handler::pid(), Cast::term()) -&gt; ok | no_return()</pre>
<br></br>




<p>Send an asynchronous UBF message.</p>
<a name="start_handler-5"></a>

###start_handler/5##




`start_handler(MetaMod, Mod, Server, StatelessRPC, SpawnOpts) -> any()`

<a name="start_manager-2"></a>

###start_manager/2##




`start_manager(Mod, Args) -> any()`


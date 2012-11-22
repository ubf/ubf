

#Module contract_manager#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Contract manager server</p>


<pre><code>This module implements the contract manager server process, which
runs on the Erlang server side, between the UBF driver (or the
driver for whatever protocol is being used "over the wire",
e.g. JSON-RPC) and the plugin handler server.</code></pre>



<pre><code>image:ubf-flow-01.png[UBF Flow]</code></pre>



<pre><code>== Message Passing</code></pre>



<pre><code>In the diagram below, the "Client" is actually the UBF driver
(using UBF, EBF, JSON, JSON-RPC, or other transport protocol) that
acts on behalf of the remote client.  The "Server" is actually the
plugin handler server, which acts as an intermediary between the
actual server application.</code></pre>



<pre><code>------
 Client                     Contract                    Server
   |                           |                          |
   |                           |                          |
   |                           |                          |
   |   {Driver,{rpc,Q}}        |                          |
   +---------->----------------+     {Contract,Q}         |
   |                           +------------->------------+
   |                           |                          |
   |                           |                          |
   |                           |      {reply,R,S1}        |
   |                           +-------------<------------+
   |  {Contract,{reply,R,S1}}  |                          |
   +----------<----------------+                          |
   |                           |                          |
 ............................................................
   |                           |                          |
   |                           |      {event_out,M}       |
   |                           +-------------<------------+
   |  {Contract,{event_out,M}} |                          |
   +----------<----------------+                          |
   |                           |                          |
 ............................................................
   |                           |                          |
   |  {Contract,{event_in,M}}  |                          |
   +---------->----------------+                          |
   |                           |      {event_in,M}        |
   |                           +------------->------------+
   |                           |                          |
------</code></pre>
.

<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#do_eventIn-4">do_eventIn/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_eventOut-4">do_eventOut/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_lpcIn-4">do_lpcIn/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_lpcOut-9">do_lpcOut/9</a></td><td></td></tr><tr><td valign="top"><a href="#do_lpcOutError-6">do_lpcOutError/6</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpcIn-4">do_rpcIn/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpcOut-9">do_rpcOut/9</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpcOutError-5">do_rpcOutError/5</a></td><td></td></tr><tr><td valign="top"><a href="#do_rpcOutError-6">do_rpcOutError/6</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="do_eventIn-4"></a>

###do_eventIn/4##


`do_eventIn(Msg, State, Mod, TLogMod) -> any()`

<a name="do_eventOut-4"></a>

###do_eventOut/4##


`do_eventOut(Msg, State, Mod, TLogMod) -> any()`

<a name="do_lpcIn-4"></a>

###do_lpcIn/4##


`do_lpcIn(Q, State, Mod, TLogMod) -> any()`

<a name="do_lpcOut-9"></a>

###do_lpcOut/9##


`do_lpcOut(Ref, Q, State, Mod, Reply, ReplyState, NewState, NewMod, TLogMod) -> any()`

<a name="do_lpcOutError-6"></a>

###do_lpcOutError/6##


`do_lpcOutError(Ref, Q, State, Mod, Error, TLogMod) -> any()`

<a name="do_rpcIn-4"></a>

###do_rpcIn/4##


`do_rpcIn(Q, State, Mod, TLogMod) -> any()`

<a name="do_rpcOut-9"></a>

###do_rpcOut/9##


`do_rpcOut(Ref, Q, State, Mod, Reply, ReplyState, NewState, NewMod, TLogMod) -> any()`

<a name="do_rpcOutError-5"></a>

###do_rpcOutError/5##


`do_rpcOutError(Q, State, Mod, Error, TLogMod) -> any()`

<a name="do_rpcOutError-6"></a>

###do_rpcOutError/6##


`do_rpcOutError(Ref, Q, State, Mod, Error, TLogMod) -> any()`

<a name="start-1"></a>

###start/1##


<pre>start(SpawnOpts::list()) -&gt; pid()</pre>
<br></br>


<a name="start-3"></a>

###start/3##


<pre>start(SimpleRPC::boolean(), VerboseRPC::boolean(), SpawnOpts::list()) -&gt; pid()</pre>
<br></br>



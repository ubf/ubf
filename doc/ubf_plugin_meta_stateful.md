

#Abstract module ubf_plugin_meta_stateful [MODULES]#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Implement the UBF(C) meta-protocol for UBF(B) "stateful" contracts.

<a name="description"></a>

##Description##




The metaprotocol is used at the beginning of a UBF session to
select one of the UBF(B) contracts that the TCP listener is
capable of offering.  The list of contracts (or more precisely,
the Erlang modules that implement the contract(s)) is passed via
the `ubf_server:start_link()` function, in the `PluginModule`   
list.



Code in this module is executed by the "Plugin Handler" process   
in the Process Structure Diagram in the Overview.

For the purposes of this module, the list of modules that
implement contracts is passed using Erlang parameterized module
`Module:new(ModuleList)` syntax.  See the Erlang/OTP documentation
for more information on parameterized module syntax and usage.
For code examples, look in the
"[../test/unit](../test/unit)"
directory for several examples (see files with "_plugin.erl" suffix).
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#description-0">description/0</a></td><td>Emit a description string.</td></tr><tr><td valign="top"><a href="#handlerRpc-4">handlerRpc/4</a></td><td>Required UBF contract implementation callback: call an RPC function.</td></tr><tr><td valign="top"><a href="#handlerStart-2">handlerStart/2</a></td><td>Required UBF contract implementation callback: start a new session
handler process.</td></tr><tr><td valign="top"><a href="#handlerStop-3">handlerStop/3</a></td><td>Required UBF contract implementation callback: stop a session
handler process.</td></tr><tr><td valign="top"><a href="#info-0">info/0</a></td><td>Emit an info string.</td></tr><tr><td valign="top"><a href="#managerRestart-2">managerRestart/2</a></td><td>Required UBF contract implementation callback: restart a manager
process.</td></tr><tr><td valign="top"><a href="#managerRpc-2">managerRpc/2</a></td><td>Required UBF contract implementation callback: call a manager's RPC
function.</td></tr><tr><td valign="top"><a href="#managerStart-1">managerStart/1</a></td><td>Required UBF contract implementation callback: start manager
process(es).</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="description-0"></a>

###description/0##




`description() -> any()`



Emit a description string.<a name="handlerRpc-4"></a>

###handlerRpc/4##




<pre>handlerRpc(StateName::atom(), RpcCall::term(), StateData::term(), ManagerPid::pid()) -&gt; {Reply::term(), NewStateName::atom(), NewStateData::term()} | {changeContract, Reply::term(), NewStateData::term(), HandlerMod::atom(), State1::term(), Data1::term(), ManagerPid::pid()}</pre>
<br></br>




Required UBF contract implementation callback: call an RPC function.<a name="handlerStart-2"></a>

###handlerStart/2##




<pre>handlerStart(Arg_From_UBF_Client::term(), ManagerProc::pid()) -&gt; {accept, Reply::term(), StateName::atom(), StateData::term()} | {reject, Reply::term()}</pre>
<br></br>




Required UBF contract implementation callback: start a new session
handler process.<a name="handlerStop-3"></a>

###handlerStop/3##




<pre>handlerStop(Env::term(), Reason::term(), StateData::term()) -&gt; [void()](#type-void)</pre>
<br></br>




Required UBF contract implementation callback: stop a session
handler process.<a name="info-0"></a>

###info/0##




`info() -> any()`



Emit an info string.<a name="managerRestart-2"></a>

###managerRestart/2##




<pre>managerRestart(Args::term(), Manager::pid()) -&gt; ok | {error, Reason::term()}</pre>
<br></br>




Required UBF contract implementation callback: restart a manager
process.<a name="managerRpc-2"></a>

###managerRpc/2##




<pre>managerRpc(Args::term(), Manager::pid()) -&gt; ok | {error, Reason::term()}</pre>
<br></br>




Required UBF contract implementation callback: call a manager's RPC
function.<a name="managerStart-1"></a>

###managerStart/1##




<pre>managerStart(Args::term()) -&gt; {ok, State::term()}</pre>
<br></br>




Required UBF contract implementation callback: start manager
process(es).
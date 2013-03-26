

# Abstract module ubf_plugin_meta_stateful [MODULES] #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Implement the UBF(c) meta-protocol for UBF(b) "stateful"
contracts.</p>


<pre><code>The metaprotocol is used at the beginning of a UBF session to
select one of the UBF(b) contracts that the TCP listener is
capable of offering.  The list of contracts (or more precisely,
the Erlang modules that implement the contract(s)) is passed via
the +ubf_server:start_link()+ function, in the +PluginModule+
list.</code></pre>



<pre><code>Code in this module is executed by the "Plugin Handler" process in
the Process Structure Diagram in the Overview.</code></pre>



<pre><code>For the purposes of this module, the list of modules that
implement contracts is passed using parameterized module
+Module:new(ModuleList)+ syntax.</code></pre>
.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#description-0">description/0</a></td><td><p>Emit a description string.</p>.</td></tr><tr><td valign="top"><a href="#handlerRpc-4">handlerRpc/4</a></td><td><p>Required UBF contract implementation callback: call an RPC function.</p>.</td></tr><tr><td valign="top"><a href="#handlerStart-2">handlerStart/2</a></td><td><p>Required UBF contract implementation callback: start a new session
handler process.</p>.</td></tr><tr><td valign="top"><a href="#handlerStop-3">handlerStop/3</a></td><td><p>Required UBF contract implementation callback: stop a session
handler process.</p>.</td></tr><tr><td valign="top"><a href="#info-0">info/0</a></td><td><p>Emit an info string.</p>.</td></tr><tr><td valign="top"><a href="#managerRestart-2">managerRestart/2</a></td><td><p>Required UBF contract implementation callback: restart a manager
process.</p>.</td></tr><tr><td valign="top"><a href="#managerRpc-2">managerRpc/2</a></td><td><p>Required UBF contract implementation callback: call a manager's RPC
function.</p>.</td></tr><tr><td valign="top"><a href="#managerStart-1">managerStart/1</a></td><td><p>Required UBF contract implementation callback: start manager
process(es).</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="description-0"></a>

### description/0 ###

`description() -> any()`

<p>Emit a description string.</p>

<a name="handlerRpc-4"></a>

### handlerRpc/4 ###

`handlerRpc(State, X2, Data, Manager) -> any()`

<p>Required UBF contract implementation callback: call an RPC function.</p>

<a name="handlerStart-2"></a>

### handlerStart/2 ###

`handlerStart(X1, X2) -> any()`

<p>Required UBF contract implementation callback: start a new session
handler process.</p>

<a name="handlerStop-3"></a>

### handlerStop/3 ###

`handlerStop(Pid, Reason, ManagerData) -> any()`

<p>Required UBF contract implementation callback: stop a session
handler process.</p>

<a name="info-0"></a>

### info/0 ###

`info() -> any()`

<p>Emit an info string.</p>

<a name="managerRestart-2"></a>

### managerRestart/2 ###

`managerRestart(Args, Manager) -> any()`

<p>Required UBF contract implementation callback: restart a manager
process.</p>

<a name="managerRpc-2"></a>

### managerRpc/2 ###

`managerRpc(X1, S) -> any()`

<p>Required UBF contract implementation callback: call a manager's RPC
function.</p>

<a name="managerStart-1"></a>

### managerStart/1 ###

`managerStart(Args) -> any()`

<p>Required UBF contract implementation callback: start manager
process(es).</p>


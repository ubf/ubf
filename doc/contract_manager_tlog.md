

# Module contract_manager_tlog #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Contract manager transaction logging.</p>


<pre><code>This module can be used by the server to log individual protocol
transactions, if desired.  This simple default module uses the
Erlang/OTP +error_logger+ module; we highly recommend that the
+sasl+ application be running to take full advantage of OTP\'s
error and event handling capabilities.</code></pre>
.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#eventIn-5">eventIn/5</a></td><td></td></tr><tr><td valign="top"><a href="#eventOut-5">eventOut/5</a></td><td></td></tr><tr><td valign="top"><a href="#lpcIn-4">lpcIn/4</a></td><td></td></tr><tr><td valign="top"><a href="#lpcOut-9">lpcOut/9</a></td><td></td></tr><tr><td valign="top"><a href="#lpcOutError-6">lpcOutError/6</a></td><td></td></tr><tr><td valign="top"><a href="#rpcFinish-1">rpcFinish/1</a></td><td></td></tr><tr><td valign="top"><a href="#rpcIn-4">rpcIn/4</a></td><td></td></tr><tr><td valign="top"><a href="#rpcOut-9">rpcOut/9</a></td><td></td></tr><tr><td valign="top"><a href="#rpcOutError-5">rpcOutError/5</a></td><td></td></tr><tr><td valign="top"><a href="#rpcOutError-6">rpcOutError/6</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="eventIn-5"></a>

### eventIn/5 ###

`eventIn(TLogMod_x, Msg, State, Mod, Status) -> any()`


<a name="eventOut-5"></a>

### eventOut/5 ###

`eventOut(TLogMod_x, Msg, State, Mod, Status) -> any()`


<a name="lpcIn-4"></a>

### lpcIn/4 ###

`lpcIn(TLogMod_x, Q, State, Mod) -> any()`


<a name="lpcOut-9"></a>

### lpcOut/9 ###

`lpcOut(TLogMod_x, StartTime, Q, State, Mod, Reply, NewState, NewMod, Status) -> any()`


<a name="lpcOutError-6"></a>

### lpcOutError/6 ###

`lpcOutError(TLogMod_x, StartTime, Q, State, Mod, Error) -> any()`


<a name="rpcFinish-1"></a>

### rpcFinish/1 ###

`rpcFinish(TLog) -> any()`


<a name="rpcIn-4"></a>

### rpcIn/4 ###

`rpcIn(TLogMod_x, Q, State, Mod) -> any()`


<a name="rpcOut-9"></a>

### rpcOut/9 ###

`rpcOut(TLogMod_x, StartTime, Q, State, Mod, Reply, NewState, NewMod, Status) -> any()`


<a name="rpcOutError-5"></a>

### rpcOutError/5 ###

`rpcOutError(TLogMod_x, Q, State, Mod, Error) -> any()`


<a name="rpcOutError-6"></a>

### rpcOutError/6 ###

`rpcOutError(TLogMod_x, StartTime, Q, State, Mod, Error) -> any()`



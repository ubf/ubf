

#Module contract_driver#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Generic protocol driver process for protocol sessions.</p>.



__This module defines the `contract_driver` behaviour.__
<br></br>
 Required callback functions: `start/1`, `start/2`, `init/1`, `init/2`, `encode/3`, `decode/5`.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#loop-5">loop/5</a></td><td></td></tr><tr><td valign="top"><a href="#loop-6">loop/6</a></td><td></td></tr><tr><td valign="top"><a href="#loop-7">loop/7</a></td><td><p>Driver main loop.</p>
<ul>
<li>
<p>
A driver sits between a socket and a Pid
</p>
</li>
<li>
<p>
Stuff on the socket is send to the Pid
</p>
</li>
<li>
<p>
Stuff from the Pid is send to the socket
</p>
</li>
<li>
<p>
When it is called the Socket has been set to send messages to the
driver and the Pid exists
</p>
</li>
<li>
<p>
If one side dies the process dies
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#relay-3">relay/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="behaviour_info-1"></a>

###behaviour_info/1##




`behaviour_info(X1) -> any()`

<a name="loop-5"></a>

###loop/5##




`loop(Module, Contract, Options, Pid, Socket) -> any()`

<a name="loop-6"></a>

###loop/6##




`loop(Module, Contract, Options, Pid, Socket, Timeout) -> any()`

<a name="loop-7"></a>

###loop/7##




`loop(Module, Contract, Options, Pid, Socket, Timeout, Cont) -> any()`



<p>Driver main loop.</p>
<ul>
<li>
<p>
A driver sits between a socket and a Pid
</p>
</li>
<li>
<p>
Stuff on the socket is send to the Pid
</p>
</li>
<li>
<p>
Stuff from the Pid is send to the socket
</p>
</li>
<li>
<p>
When it is called the Socket has been set to send messages to the
driver and the Pid exists
</p>
</li>
<li>
<p>
If one side dies the process dies
</p>
</li>
</ul>
<a name="relay-3"></a>

###relay/3##




`relay(Module, Pid, Pid1) -> any()`

<a name="start-3"></a>

###start/3##




`start(Module, Contract, Options) -> any()`




#Module ebf_driver#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Protocol driver process for EBF (Erlang Binary Format)
protocol sessions.</p>


<pre><tt>This driver automagically relies on the OTP +gen_tcp+ "packet"
feature, using a 4-byte prefix to specify the size of the data
coming from the client.  Similarly, this packet feature is used
when sending our reply back to the client.</tt></pre>
.



__Behaviours:__ [`contract_driver`](contract_driver.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-5">decode/5</a></td><td></td></tr><tr><td valign="top"><a href="#encode-3">encode/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="decode-5"></a>

###decode/5##




`decode(Contract, Options, X3, Binary, CallBack) -> any()`

<a name="encode-3"></a>

###encode/3##




`encode(Contract, Options, Term) -> any()`

<a name="init-1"></a>

###init/1##




`init(Contract) -> any()`

<a name="init-2"></a>

###init/2##




`init(Contract, Options) -> any()`

<a name="start-1"></a>

###start/1##




`start(Contract) -> any()`

<a name="start-2"></a>

###start/2##




`start(Contract, Options) -> any()`


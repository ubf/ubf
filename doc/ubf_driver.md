

#Module ubf_driver#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Protocol driver process for UBF(A) protocol sessions.



__Behaviours:__ [`contract_driver`](contract_driver.md).<a name="description"></a>

##Description##


The process executing `loop()` in this module is represented in the
diagram below by the "UBF Driver" circle.
![ubf-flow-01.png](ubf-flow-01.png)<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-5">decode/5</a></td><td></td></tr><tr><td valign="top"><a href="#encode-3">encode/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="decode-5"></a>

###decode/5##




`decode(Contract, Safe, Cont, Binary, CallBack) -> any()`

<a name="encode-3"></a>

###encode/3##




`encode(Contract, Safe, Term) -> any()`

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


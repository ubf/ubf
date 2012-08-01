

#Module ebf#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Low-level functions for encoding and decoding the UBF(a)
protocol for EBF.</p>.

__Behaviours:__ [`contract_proto`](contract_proto.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-3">decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-0">decode_init/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-1">decode_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-2">decode_init/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#proto_driver-0">proto_driver/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_packet_type-0">proto_packet_type/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_vsn-0">proto_vsn/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="decode-1"></a>

###decode/1##


<pre>decode(X::binary()) -&gt; no_return()</pre>
<br></br>


<a name="decode-2"></a>

###decode/2##


<pre>decode(X::binary(), Mod::module()) -&gt; no_return()</pre>
<br></br>


<a name="decode-3"></a>

###decode/3##


<pre>decode(X::binary(), Mod::module(), Cont::term()) -&gt; no_return()</pre>
<br></br>


<a name="decode_init-0"></a>

###decode_init/0##


<pre>decode_init() -&gt; no_return()</pre>
<br></br>


<a name="decode_init-1"></a>

###decode_init/1##


<pre>decode_init(Safe::boolean()) -&gt; no_return()</pre>
<br></br>


<a name="decode_init-2"></a>

###decode_init/2##


<pre>decode_init(Safe::boolean(), Binary::binary()) -&gt; no_return()</pre>
<br></br>


<a name="encode-1"></a>

###encode/1##


<pre>encode(X::term()) -&gt; no_return()</pre>
<br></br>


<a name="encode-2"></a>

###encode/2##


<pre>encode(X::term(), Mod::module()) -&gt; no_return()</pre>
<br></br>


<a name="proto_driver-0"></a>

###proto_driver/0##


`proto_driver() -> any()`

<a name="proto_packet_type-0"></a>

###proto_packet_type/0##


`proto_packet_type() -> any()`

<a name="proto_vsn-0"></a>

###proto_vsn/0##


`proto_vsn() -> any()`


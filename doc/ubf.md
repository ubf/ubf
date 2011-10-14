

#Module ubf#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Low-level functions for encoding and decoding the UBF(a)
protocol.</p>


<pre><tt>UBF is a family of languages for transporting and describing
complex data structures across a network.  It has three
components.  In terms of a protocol stack, UBF(a) is a data
transport format, roughly equivalent to well-formed XML.</tt></pre>



<pre><tt>UBF(a) is the transport format, it was designed to be easy to
parse and to be easy to write with a text editor. UBF(a) is based
on a byte encoded virtual machine, 26 byte codes are
reserved. Instead of allocating the byte codes from 0 we use the
printable character codes to make the format easy to read.</tt></pre>
.



__Behaviours:__ [`contract_proto`](contract_proto.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deabstract-1">deabstract/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-3">decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-0">decode_init/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-1">decode_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-2">decode_init/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#proto_driver-0">proto_driver/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_packet_type-0">proto_packet_type/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_vsn-0">proto_vsn/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="deabstract-1"></a>

###deabstract/1##




`deabstract(T) -> any()`

<a name="decode-1"></a>

###decode/1##




`decode(String) -> any()`

<a name="decode-2"></a>

###decode/2##




`decode(String, Mod) -> any()`

<a name="decode-3"></a>

###decode/3##




`decode(S, Mod, X3) -> any()`

<a name="decode_init-0"></a>

###decode_init/0##




`decode_init() -> any()`

<a name="decode_init-1"></a>

###decode_init/1##




`decode_init(Safe) -> any()`

<a name="decode_init-2"></a>

###decode_init/2##




`decode_init(Safe, String) -> any()`

<a name="encode-1"></a>

###encode/1##




`encode(X) -> any()`

<a name="encode-2"></a>

###encode/2##




`encode(X, Mod) -> any()`

<a name="proto_driver-0"></a>

###proto_driver/0##




`proto_driver() -> any()`

<a name="proto_packet_type-0"></a>

###proto_packet_type/0##




`proto_packet_type() -> any()`

<a name="proto_vsn-0"></a>

###proto_vsn/0##




`proto_vsn() -> any()`




#Module contract_parser#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>UBF(b) contract parser.</p>


<pre><code>Parsing a UBF(b) contract is done via a compiler "parse transform"
during the usual compilation of an Erlang source module.</code></pre>
.

<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#builtin_types-0">builtin_types/0</a></td><td></td></tr><tr><td valign="top"><a href="#builtin_types-1">builtin_types/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_stream-3">parse_stream/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_transform-5">parse_transform/5</a></td><td></td></tr><tr><td valign="top"><a href="#parse_transform_contract-2">parse_transform_contract/2</a></td><td></td></tr><tr><td valign="top"><a href="#predefined_types-0">predefined_types/0</a></td><td></td></tr><tr><td valign="top"><a href="#predefined_types-1">predefined_types/1</a></td><td></td></tr><tr><td valign="top"><a href="#tags-1">tags/1</a></td><td></td></tr><tr><td valign="top"><a href="#tags-2">tags/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="builtin_types-0"></a>

###builtin_types/0##


`builtin_types() -> any()`

<a name="builtin_types-1"></a>

###builtin_types/1##


`builtin_types(X1) -> any()`

<a name="parse_stream-3"></a>

###parse_stream/3##


`parse_stream(Stream, Lex, Yecc) -> any()`

<a name="parse_transform-2"></a>

###parse_transform/2##


`parse_transform(In, Opts) -> any()`

<a name="parse_transform-5"></a>

###parse_transform/5##


`parse_transform(In, Opts, File, Imports, ParseFun) -> any()`

<a name="parse_transform_contract-2"></a>

###parse_transform_contract/2##


`parse_transform_contract(In, Contract) -> any()`

<a name="predefined_types-0"></a>

###predefined_types/0##


`predefined_types() -> any()`

<a name="predefined_types-1"></a>

###predefined_types/1##


`predefined_types(X1) -> any()`

<a name="tags-1"></a>

###tags/1##


`tags(P1) -> any()`

<a name="tags-2"></a>

###tags/2##


`tags(P1, Imports) -> any()`


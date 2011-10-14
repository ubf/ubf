

#Module contract_parser#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>UBF(b) contract parser.</p>


<pre><tt>Parsing a UBF(b) contract is done via a compiler "parse transform"
during the usual compilation of an Erlang source module.</tt></pre>
.

<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make-0">make/0</a></td><td></td></tr><tr><td valign="top"><a href="#make_lex-0">make_lex/0</a></td><td></td></tr><tr><td valign="top"><a href="#make_yecc-0">make_yecc/0</a></td><td></td></tr><tr><td valign="top"><a href="#parse_file-1">parse_file/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_transform_contract-2">parse_transform_contract/2</a></td><td></td></tr><tr><td valign="top"><a href="#preDefinedTypes-0">preDefinedTypes/0</a></td><td></td></tr><tr><td valign="top"><a href="#preDefinedTypesWithAttrs-0">preDefinedTypesWithAttrs/0</a></td><td></td></tr><tr><td valign="top"><a href="#preDefinedTypesWithoutAttrs-0">preDefinedTypesWithoutAttrs/0</a></td><td></td></tr><tr><td valign="top"><a href="#tags-1">tags/1</a></td><td></td></tr><tr><td valign="top"><a href="#tags-2">tags/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="make-0"></a>

###make/0##




`make() -> any()`

<a name="make_lex-0"></a>

###make_lex/0##




`make_lex() -> any()`

<a name="make_yecc-0"></a>

###make_yecc/0##




`make_yecc() -> any()`

<a name="parse_file-1"></a>

###parse_file/1##




`parse_file(F) -> any()`

<a name="parse_transform-2"></a>

###parse_transform/2##




`parse_transform(In, Opts) -> any()`

<a name="parse_transform_contract-2"></a>

###parse_transform_contract/2##




`parse_transform_contract(In, Contract) -> any()`

<a name="preDefinedTypes-0"></a>

###preDefinedTypes/0##




`preDefinedTypes() -> any()`

<a name="preDefinedTypesWithAttrs-0"></a>

###preDefinedTypesWithAttrs/0##




`preDefinedTypesWithAttrs() -> any()`

<a name="preDefinedTypesWithoutAttrs-0"></a>

###preDefinedTypesWithoutAttrs/0##




`preDefinedTypesWithoutAttrs() -> any()`

<a name="tags-1"></a>

###tags/1##




`tags(P1) -> any()`

<a name="tags-2"></a>

###tags/2##




`tags(P1, Imports) -> any()`




#Module contract_yecc#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

##Data Types##




###<a name="type-yecc_ret">yecc_ret()</a>##



<pre>yecc_ret() = {error, term()} | {ok, term()}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_and_scan-1">parse_and_scan/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format_error-1"></a>

###format_error/1##


<pre>format_error(Message::any()) -&gt; [char() | list()]</pre>
<br></br>


<a name="parse-1"></a>

###parse/1##


<pre>parse(Tokens::list()) -> <a href="#type-yecc_ret">yecc_ret()</a></pre>
<br></br>


<a name="parse_and_scan-1"></a>

###parse_and_scan/1##


<pre>parse_and_scan(X1::{function() | {atom(), atom()}, [term()]} | {atom(), atom(), [term()]}) -> <a href="#type-yecc_ret">yecc_ret()</a></pre>
<br></br>



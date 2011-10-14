

#Module contracts#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Contract implementation: compare a term against a contract.</p>


<pre><tt>See the function checkType/3 for assistance on checking if a term
does/does not break a contract.</tt></pre>
.

<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#checkEventIn-3">checkEventIn/3</a></td><td></td></tr><tr><td valign="top"><a href="#checkEventOut-3">checkEventOut/3</a></td><td></td></tr><tr><td valign="top"><a href="#checkRPCIn-3">checkRPCIn/3</a></td><td></td></tr><tr><td valign="top"><a href="#checkRPCOut-4">checkRPCOut/4</a></td><td></td></tr><tr><td valign="top"><a href="#checkType-3">checkType/3</a></td><td><p>Given a contract type name, a term to check against that
contract type, and a contract module name, verify the term against
that contract's type.</p>


<pre><tt>Example usage from the irc_plugin.con contract:</tt></pre>



<pre><tt>------
1> contracts:checkType(ok, ok, irc_plugin).
2> contracts:checkType(bool, true, irc_plugin).
3> contracts:checkType(nick, {'#S', "foo"}, irc_plugin).
4> contracts:checkType(joinEvent, {joins, {'#S', "nck"}, {'#S', "grp"}}, irc_plugin).
5> contracts:checkType(joinEvent, {joins, {'#S', "nck"}, {'#S', bad_atom}}, irc_plugin).
------</tt></pre>



<pre><tt>NOTE: This is a brute-force function, but it works, mostly.  Don\'t
try to have a computer parse the output in error cases: the failure
output is meant only for human eyes.</tt></pre>
.</td></tr><tr><td valign="top"><a href="#isType-3">isType/3</a></td><td></td></tr><tr><td valign="top"><a href="#isTypeAttr-2">isTypeAttr/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="checkEventIn-3"></a>

###checkEventIn/3##




`checkEventIn(Msg, ThisState, Mod) -> any()`

<a name="checkEventOut-3"></a>

###checkEventOut/3##




`checkEventOut(Msg, ThisState, Mod) -> any()`

<a name="checkRPCIn-3"></a>

###checkRPCIn/3##




`checkRPCIn(Msg, State, Mod) -> any()`

<a name="checkRPCOut-4"></a>

###checkRPCOut/4##




`checkRPCOut(MsgOut, StateOut, FSM2, Mod) -> any()`

<a name="checkType-3"></a>

###checkType/3##




<pre>checkType(HumanType::atom(), Term::term(), Mod::module()) -&gt; yup | term()</pre>
<br></br>




<p>Given a contract type name, a term to check against that
contract type, and a contract module name, verify the term against
that contract's type.</p>


<pre><tt>Example usage from the irc_plugin.con contract:</tt></pre>



<pre><tt>------
1> contracts:checkType(ok, ok, irc_plugin).
2> contracts:checkType(bool, true, irc_plugin).
3> contracts:checkType(nick, {'#S', "foo"}, irc_plugin).
4> contracts:checkType(joinEvent, {joins, {'#S', "nck"}, {'#S', "grp"}}, irc_plugin).
5> contracts:checkType(joinEvent, {joins, {'#S', "nck"}, {'#S', bad_atom}}, irc_plugin).
------</tt></pre>



<pre><tt>NOTE: This is a brute-force function, but it works, mostly.  Don\'t
try to have a computer parse the output in error cases: the failure
output is meant only for human eyes.</tt></pre>

<a name="isType-3"></a>

###isType/3##




`isType(Type, X, Mod) -> any()`

<a name="isTypeAttr-2"></a>

###isTypeAttr/2##




`isTypeAttr(X1, X2) -> any()`


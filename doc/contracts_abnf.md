

#Module contracts_abnf#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Contract checking assistants for ABNF.</p>.

<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_binary-4">check_binary/4</a></td><td><p>Parse (and validate) the given binary against abnf contract
types.  It is straightforward to extend this implementation to
return the parsed abnf types stack for parsed (and invalid)
binaries.  The abnf types with sub-binary annotations could then be
used by other applications that need the abnf bits.  However, this
feature is beyond the current scope and goals of this package.</p>.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="check_binary-4"></a>

###check_binary/4##




`check_binary(Check, X, Level, Mod) -> any()`



<p>Parse (and validate) the given binary against abnf contract
types.  It is straightforward to extend this implementation to
return the parsed abnf types stack for parsed (and invalid)
binaries.  The abnf types with sub-binary annotations could then be
used by other applications that need the abnf bits.  However, this
feature is beyond the current scope and goals of this package.</p>

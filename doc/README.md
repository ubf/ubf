

#Universal Binary Format#


Copyright (c) 2011-2012 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>This is UBF, a framework that permits Erlang and the outside world to
talk with each other.  This repository is based on Joe Armstrong's
original UBF code with an MIT license file added to the distribution.
Since then, a large number of enhancements and improvements have been
added.</p>
<p><em>This repository is intended for production deployment and is deployed
in "24x7x365" carrier-grade systems.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download, build, and test the ubf application in one shot, please
follow this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/ubf/ubf.git ubf
$ cd ubf
$ make deps clean compile test</code></pre>

<p>For an alternative recipe with other "features" albeit more complex,
please read further.</p>



<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is a good first step.</p>
<p>The UBF User's Guide is the best next step.  Check out
<a href="http://ubf.github.com/ubf/ubf-user-guide.en.html">http://ubf.github.com/ubf/ubf-user-guide.en.html</a> for further
detailed information.</p>
<p>One of the better places to start is to look in the "doc" directory.
See the "Reference Documentation" section for suggestions on where to
find greater detail.</p>
<p>The unit tests in the "test/unit" directory provide small examples of
how to use all of the public API.  In particular, the <strong>client</strong>.erl
files contain comments at the top with a list of prerequisites and
small examples, recipe-style, for starting each server and using the
client.</p>
<p>The eunit tests in the "test/eunit" directory perform several smoke
and error handling uses cases.  The stateless_plugin and
stateful_plugin test applications are concrete examples on how to
integrate one or more UBF listeners into an Erlang/OTP application.</p>


<h3 id="_what_is_ubf">What is UBF?</h3>
<p>UBF is the "Universal Binary Format", designed and implemented by Joe
Armstrong.  UBF is a language for transporting and describing complex
data structures across a network.  It has three components:</p>
<ul>
<li>
<p>
UBF(a) is a "language neutral" data transport format, roughly
  equivalent to well-formed XML.
</p>
</li>
<li>
<p>
UBF(b) is a programming language for describing types in UBF(a) and
  protocols between clients and servers.  This layer is typically
  called the "protocol contract".  UBF(b) is roughly equivalent to
  Verified XML, XML-schemas, SOAP and WDSL.
</p>
</li>
<li>
<p>
UBF(c) is a meta-level protocol used between a UBF client and a UBF
  server.
</p>
</li>
</ul>
<p>See <a href="http://ubf.github.com/ubf">http://ubf.github.com/ubf</a> for further details.</p>


<h3 id="_what_is_ebf">What is EBF?</h3>
<p>EBF is an implementation of UBF(b) but does not use UBF(a) for client
and server communication.  Instead, Erlang-style conventions are used
instead:</p>
<ul>
<li>
<p>
Structured terms are serialized via the Erlang BIFs <code>term_to_binary()</code>
  and <code>binary_to_term()</code>.
</p>
</li>
<li>
<p>
Terms are framed using the <code>gen_tcp</code> <code>{packet, 4}</code> format: a 32-bit
  unsigned integer (big-endian?) specifies packet length.
</p>


<pre><code>+-------------------------+-------------------------------+
| Packet length (32 bits) | Packet data (variable length) |
+-------------------------+-------------------------------+</code></pre>

</li>
</ul>
<p>The name "EBF" is short for "Erlang Binary Format".</p>


<h3 id="_what_about_jsf_and_json_rpc">What about JSF and JSON-RPC?</h3>
<p>See the ubf-jsonrpc open source repository
<a href="https://github.com/ubf/ubf-jsonrpc">https://github.com/ubf/ubf-jsonrpc</a> for details.  ubf-jsonrpc is a
framework for integrating UBF, JSF, and JSON-RPC.</p>


<h3 id="_what_about_tbf_and_thrift">What about TBF and Thrift?</h3>
<p>See the ubf-thrift open source repository
<a href="https://github.com/ubf/ubf-thrift">https://github.com/ubf/ubf-thrift</a> for details.  ubf-thrift is a
framework for integrating UBF, TBF, and Thrift.</p>


<h3 id="_what_about_abnf">What about ABNF?</h3>
<p>See the ubf-abnf open source repository
<a href="https://github.com/ubf/ubf-abnf">https://github.com/ubf/ubf-abnf</a> for details.  ubf-abnf is a framework
for integrating UBF and ABNF.</p>


<h3 id="_what_about_eep8">What about EEP8?</h3>
<p>See the ubf-eep8 open source repository
<a href="https://github.com/ubf/ubf-eep8">https://github.com/ubf/ubf-eep8</a> for details.  ubf-eep8 is a framework
for integrating UBF and EEP8.</p>




<h2 id="_to_download">To download</h2>

<ol class="arabic">
<li>
<p>
Configure your e-mail and name for Git
</p>


<pre><code>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</code></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><code>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</code></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u https://github.com/ubf/manifests.git -m ubf-default.xml</code></pre>


<table><tr>
<td class="icon">
Note
</td>
<td class="content">Your "Git" identity is needed during the init step.  Please
enter the name and email of your GitHub account if you have one.  Team
members having read-write access are recommended to use "repo init -u
<a href="mailto:git@github.com">git@github.com</a>:ubf/manifests.git -m ubf-default-rw.xml".</td>
</tr></table>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">If you want to checkout the latest development version, please
append " -b dev" to the repo init command.</td>
</tr></table>

</li>
<li>
<p>
Download Git repositories
</p>


<pre><code>$ cd working-directory-name
$ repo sync</code></pre>

</li>
</ol>
<p>For further information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R13B04 or newer, R15B02 has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
Git - <a href="http://git-scm.com/">http://git-scm.com/</a>
</p>
<ul>
<li>
<p>
<strong>Git 1.5.4 or newer, Git 1.8.0 has been tested most recently</strong>
</p>
</li>
<li>
<p>
<em>required for Repo and GitHub</em>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
<li>
<p>
Python - <a href="http://www.python.org">http://www.python.org</a>
</p>
<ul>
<li>
<p>
<strong>Python 2.4 or newer, Python 2.7.2 has been tested most recently
    (CAUTION: Python 3.x might be too new)</strong>
</p>
</li>
<li>
<p>
<em>required for Repo</em>
</p>
</li>
</ul>
</li>
<li>
<p>
Rebar - <a href="https://github.com/rebar/rebar/wiki">https://github.com/rebar/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.html">http://source.android.com/source/git-repo.html</a>
</p>
</li>
</ul>



<h2 id="_to_build_basic_recipe">To build - basic recipe</h2>

<ol class="arabic">
<li>
<p>
Get and install an erlang system <a href="http://www.erlang.org">http://www.erlang.org</a>
</p>
</li>
<li>
<p>
Build
</p>


<pre><code>$ cd working-directory-name
$ make compile</code></pre>

</li>
<li>
<p>
Run the unit tests
</p>


<pre><code>$ cd working-directory-name
$ make eunit</code></pre>

</li>
</ol>



<h2 id="_to_build_optional_features">To build - optional features</h2>

<ol class="upperalpha">
<li>
<p>
Dialyzer Testing <em>basic recipe</em>
</p>
<ol class="arabic">
<li>
<p>
Build Dialyzer's PLT <em>(required once)</em>
</p>


<pre><code>$ cd working-directory-name
$ make build-plt</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">Check Makefile and dialyzer's documentation for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze with specs
</p>


<pre><code>$ cd working-directory-name
$ make dialyze</code></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">If you manually run dialyzer with the "-r" option, execute
"make clean compile" first to avoid finding duplicate beam files
underneath rebar's .eunit directory.  Check Makefile for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze without specs
</p>


<pre><code>$ cd working-directory-name
$ make dialyze-nospec</code></pre>

</li>
</ol>
</li>
<li>
<p>
To build the Java client and run its encoding/decoding unit test:
</p>


<pre><code>$ cd working-directory-name
$ make -C lib/ubf/priv/java</code></pre>

</li>
<li>
<p>
The Python client depends on the "py-interface" library.  To clone
   and build it, use:
</p>


<pre><code>$ cd working-directory-name
$ git clone git://repo.or.cz/py_interface.git
$ cd py_interface
$ autoconf
$ make</code></pre>

<p>Then install as a normal Python package or run using "env
PYTHONPATH=working-directory-name/py_interface python your-script.py"</p>
</li>
</ol>



<h2 id="_what_s_new_in_ubf_2_0">What's New in UBF 2.0</h2>

<p>This section highlights new features and key changes in UBF 2.0.  This
release of UBF is not backwards compatible with older versions of UBF.</p>
<ul>
<li>
<p>
The syntax for UBF(b) has been modified to align closer (but not
  identical) with Erlang's native type and spec declarations defined
  by EEP8 (<a href="http://www.erlang.org/eeps/eep-0008.html">http://www.erlang.org/eeps/eep-0008.html</a>).  A subset of
  EEP8 types are now available as UBF(b) builtin types.
</p>
</li>
<li>
<p>
The UBF(b) builtin types <code>proplist()</code> and <code>string()</code> have been
  renamed to <code>ubfproplist()</code> and <code>ubfstring()</code>, respectively.
</p>
</li>
<li>
<p>
An Erlang "header" file corresponding to each UBF(b) contract is
  automatically created in an application's ebin directory.  This
  file contains Erlang type, spec, and record declarations that can be
  included by a UBF(b) contract's implementation module or by other
  Erlang modules.
</p>
</li>
<li>
<p>
The API and internal implementation of UBF's contract parser,
  contract manager, contract driver, and contract plugin handler has
  changed (in some places).
</p>
</li>
<li>
<p>
For the above Quick Start Recipe, a Makefile has been added to
  automate and document common recipes.  This Makefile is also used
  for Travis CI (<a href="https://travis-ci.org">https://travis-ci.org</a>) integration.
</p>
</li>
</ul>



<h2 id="_roadmap">Roadmap</h2>

<ul>
<li>
<p>
QuickCheck/PropEr/Triq tests
</p>
</li>
<li>
<p>
Transport Protocols
</p>
<ul>
<li>
<p>
SUNRPC (<a href="http://tools.ietf.org.md/rfc5531">http://tools.ietf.org/html/rfc5531</a>) support
</p>
</li>
<li>
<p>
WebSockets support
</p>
</li>
<li>
<p>
More Thrift (<a href="http://incubator.apache.org/thrift/">http://incubator.apache.org/thrift/</a>) support
</p>
<ul>
<li>
<p>
Compact Format
</p>
</li>
</ul>
</li>
<li>
<p>
Protocol Buffers (<a href="http://code.google.com/apis/protocolbuffers/">http://code.google.com/apis/protocolbuffers/</a>) support
</p>
</li>
<li>
<p>
Bert-RPC (<a href="http://bert-rpc.org/">http://bert-rpc.org/</a>) support
</p>
</li>
</ul>
</li>
<li>
<p>
Misc
</p>
<ul>
<li>
<p>
Multiple listeners for a single UBF server support
</p>
</li>
<li>
<p>
UDP support
</p>
</li>
</ul>
</li>
</ul>

<table><tr>
<td class="icon">
Note
</td>
<td class="content">BERT-RPC is UBF/EBF with a specialized contract and plugin
handler implementation for BERT-RPC. UBF/EBF already supports all of
the BERT data types.  UBF is the text-based wire protocol.  EBF is the
binary-based wire protocol (based on Erlang's binary serialization
format).</td>
</tr></table>




<h2 id="_credits">Credits</h2>

<p>Many, many thanks to Joe Armstrong, UBF's designer and original
implementer.</p>
<p>Gemini Mobile Technologies, Inc. has approved the release of its
extensions, improvements, etc. under an MIT license.  Joe Armstrong
has also given his blessing to Gemini's license choice.</p>




##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="contract_driver.md" class="module">contract_driver</a></td></tr>
<tr><td><a href="contract_lex.md" class="module">contract_lex</a></td></tr>
<tr><td><a href="contract_manager.md" class="module">contract_manager</a></td></tr>
<tr><td><a href="contract_manager_tlog.md" class="module">contract_manager_tlog</a></td></tr>
<tr><td><a href="contract_parser.md" class="module">contract_parser</a></td></tr>
<tr><td><a href="contract_proto.md" class="module">contract_proto</a></td></tr>
<tr><td><a href="contract_yecc.md" class="module">contract_yecc</a></td></tr>
<tr><td><a href="contracts.md" class="module">contracts</a></td></tr>
<tr><td><a href="contracts_abnf.md" class="module">contracts_abnf</a></td></tr>
<tr><td><a href="ebf.md" class="module">ebf</a></td></tr>
<tr><td><a href="ebf_driver.md" class="module">ebf_driver</a></td></tr>
<tr><td><a href="proc_socket_server.md" class="module">proc_socket_server</a></td></tr>
<tr><td><a href="proc_utils.md" class="module">proc_utils</a></td></tr>
<tr><td><a href="qc_ubf.md" class="module">qc_ubf</a></td></tr>
<tr><td><a href="qc_ubf_impl.md" class="module">qc_ubf_impl</a></td></tr>
<tr><td><a href="qc_ubf_types.md" class="module">qc_ubf_types</a></td></tr>
<tr><td><a href="ubf.md" class="module">ubf</a></td></tr>
<tr><td><a href="ubf_client.md" class="module">ubf_client</a></td></tr>
<tr><td><a href="ubf_driver.md" class="module">ubf_driver</a></td></tr>
<tr><td><a href="ubf_plugin_handler.md" class="module">ubf_plugin_handler</a></td></tr>
<tr><td><a href="ubf_plugin_meta_stateful.md" class="module">ubf_plugin_meta_stateful</a></td></tr>
<tr><td><a href="ubf_plugin_meta_stateless.md" class="module">ubf_plugin_meta_stateless</a></td></tr>
<tr><td><a href="ubf_plugin_stateful.md" class="module">ubf_plugin_stateful</a></td></tr>
<tr><td><a href="ubf_plugin_stateless.md" class="module">ubf_plugin_stateless</a></td></tr>
<tr><td><a href="ubf_server.md" class="module">ubf_server</a></td></tr>
<tr><td><a href="ubf_types_builtin.md" class="module">ubf_types_builtin</a></td></tr>
<tr><td><a href="ubf_utils.md" class="module">ubf_utils</a></td></tr></table>




#Universal Binary Format#


Copyright (c) 2011 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>This is UBF, a framework for getting Erlang to talk to the outside
world.  This repository is based on Joe Armstrong's original UBF code
with an MIT license file added to the distribution.  Since then, a
large number of enhancements and improvements have been added.</p>
<p><em>This repository is intended for production deployment and is deployed
in carrier-grade systems.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download, build, and test the ubf application in one shot, please
follow this recipe:</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone git://github.com/norton/ubf.git ubf
$ cd ubf
$ ./rebar get-deps
$ ./rebar clean
$ ./rebar compile
$ ./rebar eunit</tt></pre>

<p>For an alternative recipe with other "features" albeit more complex,
please read further.</p>



<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is a good first step.</p>
<p>The UBF User's Guide is the best next step.  Check out
<a href="http://norton.github.com/ubf/ubf-user-guide.en.md">http://norton.github.com/ubf/ubf-user-guide.en.html</a> for further
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
and error handling uses cases.</p>
<p>The #1 most frequently asked question is: "My term X fails contract Y,
but I can't figure out why!  This X is perfectly OK.  What is going
on?"  See the the EDoc documentation for the contracts:checkType/3
function.</p>
<p>The documentation is in a state of slow improvement.  Contributions
from the wider world are welcome.  :-)</p>


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
<p>See <a href="http://norton.github.com/ubf">http://norton.github.com/ubf</a> for further details.</p>


<h3 id="_what_is_ebf">What is EBF?</h3>
<p>EBF is an implementation of UBF(b) but does not use UBF(a) for client
and server communication.  Instead, Erlang-style conventions are used
instead:</p>
<ul>
<li>
<p>
Structured terms are serialized via the Erlang BIFs <tt>term_to_binary()</tt>
  and <tt>binary_to_term()</tt>.
</p>
</li>
<li>
<p>
Terms are framed using the <tt>gen_tcp</tt> <tt>{packet, 4}</tt> format: a 32-bit
  unsigned integer (big-endian?) specifies packet length.
</p>


<pre><tt>+-------------------------+-------------------------------+
| Packet length (32 bits) | Packet data (variable length) |
+-------------------------+-------------------------------+</tt></pre>

</li>
</ul>
<p>The name "EBF" is short for "Erlang Binary Format".</p>


<h3 id="_what_about_jsf_and_json_rpc">What about JSF and JSON-RPC?</h3>
<p>See the ubf-jsonrpc open source repository
<a href="https://github.com/norton/ubf-jsonrpc">https://github.com/norton/ubf-jsonrpc</a> for details.  ubf-jsonrpc is a
framework for integrating UBF, JSF, and JSON-RPC.</p>


<h3 id="_what_about_tbf_and_thrift">What about TBF and Thrift?</h3>
<p>See the ubf-thrift open source repository
<a href="https://github.com/norton/ubf-thrift">https://github.com/norton/ubf-thrift</a> for details.  ubf-thrift is a
framework for integrating UBF, TBF, and Thrift.</p>


<h3 id="_what_about_abnf">What about ABNF?</h3>
<p>See the ubf-abnf open source repository
<a href="https://github.com/norton/ubf-abnf">https://github.com/norton/ubf-abnf</a> for details.  ubf-abnf is a
framework for integrating UBF and ABNF.</p>


<h3 id="_what_about_eep8">What about EEP8?</h3>
<p>See the ubf-eep8 open source repository
<a href="https://github.com/norton/ubf-eep8">https://github.com/norton/ubf-eep8</a> for details.  ubf-eep8 is a
framework for integrating UBF and EEP8.</p>




<h2 id="_to_download">To download</h2>

<ol class="arabic">
<li>
<p>
Configure your e-mail and name for Git
</p>


<pre><tt>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</tt></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><tt>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</tt></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u git://github.com/norton/manifests.git -m ubf-default.xml</tt></pre>


<table><tr>
<td class="icon">
Note
</td>
<td class="content">Your "Git" identity is needed during the init step.  Please
enter the name and email of your GitHub account if you have one.  Team
members having read-write access are recommended to use "repo init -u
<a href="mailto:git@github.com">git@github.com</a>:norton/manifests.git -m ubf-default-rw.xml".</td>
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


<pre><tt>$ cd working-directory-name
$ repo sync</tt></pre>

</li>
</ol>
<p>For futher information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R13B04 or newer, R15B has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.7.8 has been tested recently</strong>
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
<strong>Python 2.4 or newer, Python 2.7.1 has been tested most recently
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
Rebar - <a href="https://github.com/basho/rebar/wiki">https://github.com/basho/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.md">http://source.android.com/source/git-repo.html</a>
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


<pre><tt>$ cd working-directory-name/src
$ make compile</tt></pre>

</li>
<li>
<p>
Run the unit tests
</p>


<pre><tt>$ cd working-directory-name/src
$ make eunit</tt></pre>

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


<pre><tt>$ cd working-directory-name/src
$ make build-plt</tt></pre>


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


<pre><tt>$ cd working-directory-name/src
$ make dialyze</tt></pre>


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


<pre><tt>$ cd working-directory-name/src
$ make dialyze-nospec</tt></pre>

</li>
</ol>
</li>
<li>
<p>
To build the Java client and run its encoding/decoding unit test:
</p>


<pre><tt>$ cd working-directory-name/src
$ make -C lib/ubf/priv/java</tt></pre>

</li>
<li>
<p>
The Python client depends on the "py-interface" library.  To clone
   and build it, use:
</p>


<pre><tt>$ cd working-directory-name
$ git clone git://repo.or.cz/py_interface.git
$ cd py_interface
$ autoconf
$ make</tt></pre>

<p>Then install as a normal Python package or run using "env
PYTHONPATH=working-directory-name/py_interface python your-script.py"</p>
</li>
</ol>



<h2 id="_roadmap">Roadmap</h2>

<ul>
<li>
<p>
QuickCheck/Proper tests
</p>
</li>
<li>
<p>
Add WebSockets support
</p>
</li>
<li>
<p>
Add more Thrift (<a href="http://incubator.apache.org/thrift/">http://incubator.apache.org/thrift/</a>) support
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
Add Google's Protocol Buffers (<a href="http://code.google.com/apis/protocolbuffers/">http://code.google.com/apis/protocolbuffers/</a>) support
</p>
</li>
<li>
<p>
Add Bert-RPC (<a href="http://bert-rpc.org/">http://bert-rpc.org/</a>) support
</p>
<ul>
<li>
<p>
BERT-RPC is UBF/EBF with a specialized contract and plugin handler
    implementation for BERT-RPC. UBF/EBF already supports all of the
    BERT data types.  UBF is the text-based wire protocol.  EBF is the
    binary-based wire protocol (based on Erlang's binary
    serialization format).
</p>
</li>
</ul>
</li>
<li>
<p>
Add multiple listeners for a single UBF server support
</p>
</li>
<li>
<p>
Add UDP transport support
</p>
</li>
</ul>



<h2 id="_credits">Credits</h2>

<p>Many, many thanks to Joe Armstrong, UBF's designer and original
implementor.</p>
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
<tr><td><a href="ubf.md" class="module">ubf</a></td></tr>
<tr><td><a href="ubf_client.md" class="module">ubf_client</a></td></tr>
<tr><td><a href="ubf_driver.md" class="module">ubf_driver</a></td></tr>
<tr><td><a href="ubf_plugin_handler.md" class="module">ubf_plugin_handler</a></td></tr>
<tr><td><a href="ubf_plugin_meta_stateful.md" class="module">ubf_plugin_meta_stateful</a></td></tr>
<tr><td><a href="ubf_plugin_meta_stateless.md" class="module">ubf_plugin_meta_stateless</a></td></tr>
<tr><td><a href="ubf_plugin_stateful.md" class="module">ubf_plugin_stateful</a></td></tr>
<tr><td><a href="ubf_plugin_stateless.md" class="module">ubf_plugin_stateless</a></td></tr>
<tr><td><a href="ubf_server.md" class="module">ubf_server</a></td></tr>
<tr><td><a href="ubf_utils.md" class="module">ubf_utils</a></td></tr></table>


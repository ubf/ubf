

# Universal Binary Format #

Copyright (c) 2011-2015 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<p>This is UBF, a framework that permits Erlang and the outside world to
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


<h3 id="_tools">Tools</h3>
<p>For further information and help for related tools, please refer to
the following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R15B01 or newer, 17.0 has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.9.3 has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
</ul>




<h2 id="_what_s_new_in_ubf_2_2">What's New in UBF 2.2</h2>

<ul>
<li>
<p>
Added support for 17.0. Removed support for R14.
</p>
</li>
<li>
<p>
QuickCheck and PropEr related features and tests are unsupported
  until further notice.
</p>
</li>
<li>
<p>
Deprecate repo tool.
</p>
</li>
</ul>



<h2 id="_what_s_new_in_ubf_2_1">What's New in UBF 2.1</h2>

<p>This section highlights new features and key changes in UBF 2.1.  This
release of UBF is backwards compatible with UBF 2.0.  However, you
must recompile UBF and all applications that depend directly on UBF.</p>
<ul>
<li>
<p>
Added support for R16B. Removed support for R13B04.
</p>
</li>
<li>
<p>
Fixed issue with UBF listener's shutdown sequence.
</p>
</li>
<li>
<p>
Improved layout and presentation of the UBF User's Guide.
</p>
</li>
</ul>



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




## Modules ##


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


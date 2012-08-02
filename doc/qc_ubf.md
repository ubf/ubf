

#Abstract module qc_ubf [MOD, CONTRACTS]#
* [Function Index](#index)
* [Function Details](#functions)


__Behaviours:__ [`qc_statem`](https://github.com/norton/qc/blob/master/doc/qc_statem.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aggregate-1">aggregate/1</a></td><td></td></tr><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#command_gen-2">command_gen/2</a></td><td></td></tr><tr><td valign="top"><a href="#command_gen-3">command_gen/3</a></td><td></td></tr><tr><td valign="top"><a href="#initial_state-0">initial_state/0</a></td><td></td></tr><tr><td valign="top"><a href="#next_state-3">next_state/3</a></td><td></td></tr><tr><td valign="top"><a href="#postcondition-3">postcondition/3</a></td><td></td></tr><tr><td valign="top"><a href="#precondition-2">precondition/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_counterexample-2">qc_counterexample/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_counterexample_read-2">qc_counterexample_read/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_counterexample_write-2">qc_counterexample_write/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_prop-1">qc_prop/1</a></td><td></td></tr><tr><td valign="top"><a href="#qc_run-2">qc_run/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_sample-1">qc_sample/1</a></td><td></td></tr><tr><td valign="top"><a href="#setup-1">setup/1</a></td><td></td></tr><tr><td valign="top"><a href="#state_is_sane-1">state_is_sane/1</a></td><td></td></tr><tr><td valign="top"><a href="#teardown-1">teardown/1</a></td><td></td></tr><tr><td valign="top"><a href="#teardown-2">teardown/2</a></td><td></td></tr><tr><td valign="top"><a href="#ubf_gen_command-5">ubf_gen_command/5</a></td><td></td></tr><tr><td valign="top"><a href="#ubf_gen_command_type-5">ubf_gen_command_type/5</a></td><td></td></tr><tr><td valign="top"><a href="#ubf_rpc-3">ubf_rpc/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="aggregate-1"></a>

###aggregate/1##


`aggregate(L) -> any()`

<a name="behaviour_info-1"></a>

###behaviour_info/1##


`behaviour_info(Other) -> any()`

<a name="command_gen-2"></a>

###command_gen/2##


`command_gen(Mod, S) -> any()`

<a name="command_gen-3"></a>

###command_gen/3##


`command_gen(Mod, S, IO) -> any()`

<a name="initial_state-0"></a>

###initial_state/0##


`initial_state() -> any()`

<a name="next_state-3"></a>

###next_state/3##


`next_state(S, R, C) -> any()`

<a name="postcondition-3"></a>

###postcondition/3##


`postcondition(S, C, R) -> any()`

<a name="precondition-2"></a>

###precondition/2##


`precondition(S, C) -> any()`

<a name="qc_counterexample-2"></a>

###qc_counterexample/2##


`qc_counterexample(Options, CounterExample) -> any()`

<a name="qc_counterexample_read-2"></a>

###qc_counterexample_read/2##


`qc_counterexample_read(Options, FileName) -> any()`

<a name="qc_counterexample_write-2"></a>

###qc_counterexample_write/2##


`qc_counterexample_write(FileName, CounterExample) -> any()`

<a name="qc_prop-1"></a>

###qc_prop/1##


`qc_prop(Options) -> any()`

<a name="qc_run-2"></a>

###qc_run/2##


`qc_run(NumTests, Options) -> any()`

<a name="qc_sample-1"></a>

###qc_sample/1##


`qc_sample(Options) -> any()`

<a name="setup-1"></a>

###setup/1##


`setup(Hard) -> any()`

<a name="state_is_sane-1"></a>

###state_is_sane/1##


`state_is_sane(S) -> any()`

<a name="teardown-1"></a>

###teardown/1##


`teardown(Ref) -> any()`

<a name="teardown-2"></a>

###teardown/2##


`teardown(Ref, State) -> any()`

<a name="ubf_gen_command-5"></a>

###ubf_gen_command/5##


`ubf_gen_command(Mod, S, Contract, TypeName, TypeStack) -> any()`

<a name="ubf_gen_command_type-5"></a>

###ubf_gen_command_type/5##


`ubf_gen_command_type(Mod, S, Contract, TypeName, TypeStack) -> any()`

<a name="ubf_rpc-3"></a>

###ubf_rpc/3##


`ubf_rpc(Contract, TypeName, Type) -> any()`


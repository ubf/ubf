

# Abstract module qc_ubf_impl [MOD, CONTRACTS] #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`qc_statem`](qc_statem.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aggregate-1">aggregate/1</a></td><td></td></tr><tr><td valign="top"><a href="#command_gen-1">command_gen/1</a></td><td></td></tr><tr><td valign="top"><a href="#command_gen-2">command_gen/2</a></td><td></td></tr><tr><td valign="top"><a href="#command_typegen-4">command_typegen/4</a></td><td></td></tr><tr><td valign="top"><a href="#initial_state-1">initial_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#next_state-3">next_state/3</a></td><td></td></tr><tr><td valign="top"><a href="#postcondition-3">postcondition/3</a></td><td></td></tr><tr><td valign="top"><a href="#precondition-2">precondition/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_prop-1">qc_prop/1</a></td><td></td></tr><tr><td valign="top"><a href="#qc_run-2">qc_run/2</a></td><td></td></tr><tr><td valign="top"><a href="#qc_sample-1">qc_sample/1</a></td><td></td></tr><tr><td valign="top"><a href="#rpc-3">rpc/3</a></td><td></td></tr><tr><td valign="top"><a href="#scenario_gen-0">scenario_gen/0</a></td><td></td></tr><tr><td valign="top"><a href="#setup-0">setup/0</a></td><td></td></tr><tr><td valign="top"><a href="#setup-1">setup/1</a></td><td></td></tr><tr><td valign="top"><a href="#state_is_sane-1">state_is_sane/1</a></td><td></td></tr><tr><td valign="top"><a href="#teardown-2">teardown/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="aggregate-1"></a>

### aggregate/1 ###

`aggregate(L) -> any()`


<a name="command_gen-1"></a>

### command_gen/1 ###

`command_gen(S) -> any()`


<a name="command_gen-2"></a>

### command_gen/2 ###

`command_gen(S, IO) -> any()`


<a name="command_typegen-4"></a>

### command_typegen/4 ###

`command_typegen(S, Contract, TypeName, TypeStack) -> any()`


<a name="initial_state-1"></a>

### initial_state/1 ###

`initial_state(Scenario) -> any()`


<a name="next_state-3"></a>

### next_state/3 ###

`next_state(S, R, C) -> any()`


<a name="postcondition-3"></a>

### postcondition/3 ###

`postcondition(S, C, R) -> any()`


<a name="precondition-2"></a>

### precondition/2 ###

`precondition(S, C) -> any()`


<a name="qc_prop-1"></a>

### qc_prop/1 ###

`qc_prop(Options) -> any()`


<a name="qc_run-2"></a>

### qc_run/2 ###

`qc_run(NumTests, Options) -> any()`


<a name="qc_sample-1"></a>

### qc_sample/1 ###

`qc_sample(Options) -> any()`


<a name="rpc-3"></a>

### rpc/3 ###

`rpc(Contract, TypeName, Type) -> any()`


<a name="scenario_gen-0"></a>

### scenario_gen/0 ###

`scenario_gen() -> any()`


<a name="setup-0"></a>

### setup/0 ###

`setup() -> any()`


<a name="setup-1"></a>

### setup/1 ###

`setup(Scenario) -> any()`


<a name="state_is_sane-1"></a>

### state_is_sane/1 ###

`state_is_sane(S) -> any()`


<a name="teardown-2"></a>

### teardown/2 ###

`teardown(Ref, State) -> any()`



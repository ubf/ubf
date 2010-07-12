
##
##

from pyubf import Integer, Float, Atom, String, Binary, Tuple, List, PropList
from py_interface import erl_term

## py_interface -> pyebf
def from_py_interface(value):
    if isinstance(value, int) or isinstance(value, long):
        return Integer(value)
    elif isinstance(value, float):
        return Float(value)
    elif isinstance(value, erl_term.ErlAtom):
        if value == 'undefined':
            return None
        elif value == 'true':
            return True
        elif value == 'false':
            return False
        else:
            return Atom(str(value))
    elif isinstance(value, erl_term.ErlBinary):
        return Binary(value.contents)
    elif isinstance(value, tuple):
        if len(value) == 2 and value[0] == '#S':
            return String(value[1])
        elif len(value) == 2 and value[0] == '#P':
            l = [ [from_py_interface(v), from_py_interface(v)] for k, v in value[1] ]
            return PropList(dict(l))
        else:
            return Tuple(tuple([ from_py_interface(v) for v in list(value) ]))
    elif isinstance(value, list):
        return List([ from_py_interface(v) for v in value ])
    else:
        raise RuntimeError, 'unknown object ~s' % repr(value)

## pyebf -> py_interface
def to_py_interface(value):
    if value is None:
        return erl_term.ErlAtom("undefined")
    elif value is True:
        return erl_term.ErlAtom("true")
    elif value is False:
        return erl_term.ErlAtom("false")
    elif isinstance(value, int) or isinstance(value, long):
        return erl_term.ErlNumber(value)
    elif isinstance(value, float):
        return erl_term.ErlNumber(value)
    elif isinstance(value, Atom):
        return erl_term.ErlAtom(value)
    elif isinstance(value, String):
        return erl_term.ErlTuple(['#S', erl_term.ErlList(value)])
    elif isinstance(value, str):
        return erl_term.ErlBinary(value)
    elif isinstance(value, unicode):
        return erl_term.ErlBinary(value)
    elif isinstance(value, tuple):
        return erl_term.ErlTuple([ to_py_interface(v) for v in list(value) ])
    elif isinstance(value, list):
        return erl_term.ErlList([ to_py_interface(v) for v in value ])
    elif isinstance(value, dict):
        l = [ erl_term.ErlTuple([to_py_interface(v), to_py_interface(v)]) for k, v in value.iteritems() ]
        return erl_term.ErlTuple(['#T', erl_term.ErlList(l)])
    else:
        raise RuntimeError, 'unknown object ~s' % repr(value)

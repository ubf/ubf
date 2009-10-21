
##
## $Id$
##

from pyubf import Integer, Float, Atom, String, Binary, Record, Tuple, List, PropList

## pyjson -> pyubf
def from_pyjson(value):
    if value is None:
        return value
    elif isinstance(value, bool):
        return value
    elif isinstance(value, int) or isinstance(value, long):
        return Integer(value)
    elif isinstance(value, float):
        return Float(value)
    elif isinstance(value, str) or isinstance(value, unicode):
        return Binary(value)
    elif isinstance(value, dict):
        if value.keys() == ['$A']:
            return Atom(value['$A'])
        elif value.keys() == ['$S']:
            return String(value['$S'])
        elif value.keys() == ['$T']:
            return Tuple(tuple(from_pyjson(value['$T'])))
        elif value.has_key('$R') and len(value.keys()) > 1:
            name = value['$R']
            del value['$R']
            fields = dict([ [k, from_pyjson(v)] for k, v in value.iteritems() ])
            return Record(name, fields)
        elif value.has_key('$P') and len(value.keys()) > 1:
            l = [ [k, from_pyjson(v)] for k, v in value.iteritems() ]
            return PropList(dict(l))
        else:
            l = [ [k, from_pyjson(v)] for k, v in value.iteritems() ]
            return PropList(dict(l))        
    elif isinstance(value, list):
        return [ from_pyjson(v) for v in value ]
    else:
        raise RuntimeError, 'unknown object ~s' % repr(value)

## pyubf -> pyjson
def to_pyjson(value):
    if value is None:
        return value
    elif isinstance(value, bool):
        return value
    elif isinstance(value, int) or isinstance(value, long):
        return value
    elif isinstance(value, float):
        return value
    elif isinstance(value, Atom):
        return {'$A' : str(value)}
    elif isinstance(value, String):
        return {'$S' : str(value)}
    elif isinstance(value, str):
        return value
    elif isinstance(value, unicode):
        return value
    elif isinstance(value, tuple):
        return {'$T' : [ to_pyjson(v) for v in list(value) ]}
    elif isinstance(value, Record):
        record = dict([ [k, to_pyjson(v)] for k, v in value.iteritems() ])
        record.update({'$R' : value.name})
        return record
    elif isinstance(value, list):
        return [ to_pyjson(v) for v in value ]
    elif isinstance(value, dict):
        return dict([ [k, to_pyjson(v)] for k, v in value.iteritems() ])
    else:
        raise RuntimeError, 'unknown object ~s' % repr(value)

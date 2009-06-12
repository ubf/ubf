
##
## $Id$
##

##
## UBF primitives:
##
##   integer, float, atom, string, binary, tuple, record, list, term, void
##

def Integer(value):
    assert isinstance(value, int) or isinstance(value, long)
    return value

def Float(value):
    assert isinstance(value, float)
    return value

class Atom(str):
    def __init__(self, value):
        assert isinstance(value, str)
        str.__init__(self, value)
    def __repr__(self):
        return "<ubf-atom: %s>" % `str.__repr__(self)`

class String(str):
    def __init__(self, value):
        assert isinstance(value, str)
        str.__init__(self, value)
    def __repr__(self):
        return "<ubf-string: %s>" % `str.__repr__(self)`

def Binary(value):
    assert isinstance(value, str) or isinstance(value, unicode)
    return value

def Tuple(value):
    assert isinstance(value, tuple)
    return value

class Record(dict):
    def __init__(self, name, fields):
        assert isinstance(name, str) and isinstance(fields, dict)
        dict.__init__(self, fields)
        self.name = name
    def __repr__(self):
        return "<ubf-record: %s %s>" % (self.name, `dict.__repr__(self)`)

def List(value):
    assert isinstance(value, list)
    return value


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
        else:
            raise RuntimeError, 'unknown dict ~s' % repr(value)
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
    else:
        raise RuntimeError, 'unknown object ~s' % repr(value)

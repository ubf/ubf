
##
##

##
## UBF primitives:
##
##   integer, float, atom, string, binary, tuple, record, list, proplist, term, void
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

def PropList(value):
    assert isinstance(value, dict)
    return value

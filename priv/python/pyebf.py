###
### ebf -- A Python-implementation of an EBF client
###
###

import socket, struct, sys, types
from py_interface import erl_term
from pyubf_py_interface import to_py_interface, from_py_interface
from pyubf import Atom, Integer

class SocketError(Exception):
    pass

class EBFError(Exception):
    pass

class Socket:
    def __init__(self,host=socket.gethostname(), port=7580):
        self.host = host
        self.port = port
        self.SocketError = SocketError()
        try:
            self.sock = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
        except socket.error, msg:
            raise SocketError, 'Error in Socket Object Creation!!'

    def close(self):
        self.sock.close()

    def __str__(self):
        return 'Socket created on Host='+str(self.host)+',Port='+str(self.port)

class Client(Socket):
    def connect(self, host=socket.gethostname(), port=7580, timeout=10):
        self.host = host
        self.port = port
        self.sock.settimeout(timeout)
        try:
            self.sock.connect((self.host, self.port))
        except socket.error,msg:
            raise SocketError, 'Connection refused to '+str(self.host)+' on port '+str(self.port)

    def send(self, data):
        size = socket.htonl(len(data))
        size = struct.pack("I", size)
        self.sock.send(size)

        sent = self.sock.sendall(data)
        if sent != None:
            raise SocketError,'Connection broken to '+str(self.host)+' on port '+str(self.port)

    def recv(self):
        size = struct.calcsize("I")
        if size != 4:
            raise EBFError,'Bad "I" size '+str(size)
        size = self.sock.recv(size)
        try:
            size = socket.ntohl(struct.unpack("I", size)[0])
        except struct.error, e:
            return None

        data = ""
        while len(data) < size:
            chunk = self.sock.recv(size - len(data))
            if chunk == '':
                raise SocketError, 'Connection broken to '+str(self.host)+' on port '+str(self.port)
            data = data + chunk
        return data

    def __str__(self):
        return 'Client connected to Host=' + str(self.host) + ',Port=' + str(self.port)

class EBF(Client):
    ## TODO: add other primitive types as needed
    module = None
    timeout = None

    def atom(self, v):
        return erl_term.ErlAtom(v)

    def binary(self, v):
        return erl_term.ErlBinary(v)

    def string(self, v=""):
        return erl_term.ErlString(v)

    def ubfstring(self, v=""):
        return self.tuple([self.atom('#S'), self.string(v)])

    def tuple(self, v=[]):
        return erl_term.ErlTuple(v)

    def list(self, v=[]):
        return erl_term.ErlList(v)

    def is_atom(self, t, v=None):
        return erl_term.IsErlAtom(t) and (v is None or str(t)==v)

    def is_binary(self, t, v=None):
        return erl_term.IsErlBinary(t) and (v is None or str(t)==v)

    def is_string(self, t, v=None):
        return type(t)==types.StringType and (v is None or t==v)

    def is_ubfstring(self, t, v=None):
        return self.is_tuple(t, 2) and self.is_atom(t[0], '#S') and self.is_string(t[1], v)

    def is_tuple(self, t, v=None):
        return type(t)==types.TupleType and (v is None or len(t)==v)

    def is_list(self, t, v=None):
        return type(t)==types.ListType and (v is None or len(t)==v)

    def login(self, module, meta_server, host=socket.gethostname(), port=7580, timeout=10):
        self.connect(host, port, timeout)
        self.module = module
        self.timeout = timeout

        # read response - hello
        term = erl_term.BinaryToTerm(self.recv())
        if not self.is_tuple(term, 3):
            raise EBFError, term
        if not self.is_atom(term[0], 'ebf1.0'):
            raise EBFError, (term[0], term)
        if not self.is_ubfstring(term[1], meta_server):
            raise EBFError, (term[1], term)
        # ignore term[2]

        # write request - start session
        self.send(erl_term.TermToBinary(self.tuple([self.atom('startSession'), self.ubfstring(module), self.list()])))

        # read response - start session
        term = erl_term.BinaryToTerm(self.recv())
        if not self.is_tuple(term, 2):
            raise EBFError, term
        if not self.is_tuple(term[0], 2):
            raise EBFError, (term[0], term)
        if not self.is_atom(term[0][0], 'ok'):
            raise EBFError, (term[0][0], term)
        if not self.is_atom(term[0][1], 'ok'):
            raise EBFError, (term[0][1], term)
        if not self.is_atom(term[1], 'none'):
            raise EBFError, (term[1], term)

    ### rpc
    def rpc(self, module, request, maxsize=None, writetimeout=None, readtimeout=None):
        # TODO: implement maxsize

        if not self.module==module:
            raise EBFError, (module, self.module)

        # write request
        if writetimeout is None:
            self.sock.settimeout(self.timeout)
        else:
            self.sock.settimeout(writetimeout)

        self.send(erl_term.TermToBinary(to_py_interface(request)))

        # read response
        if readtimeout is None:
            self.sock.settimeout(self.timeout)
        else:
            self.sock.settimeout(readtimeout)
        term = erl_term.BinaryToTerm(self.recv())

        # check for client broke contract
        if self.is_tuple(term, 3) and self.is_atom(term[0], 'clientBrokeContract'):
            raise EBFError, term

        # check for server broke contract
        if self.is_tuple(term, 3) and self.is_atom(term[0], 'serverBrokeContract'):
            raise EBFError, term

        # check for server broke contract
        if not self.is_tuple(term, 2):
            raise EBFError, term

        self.sock.settimeout(self.timeout)
        return from_py_interface(term[0])

if __name__ == "__main__":
    ebf = EBF()

    ## login
    ebf.login('gdss', 'gdss_meta_server')

    ## setup
    req0 = (Atom('do'), Atom('tab1'), [(Atom('delete'), 'foo', [])], [], 1000)
    res0 = ebf.rpc('gdss', req0)

    ## get - ng
    req1 = (Atom('do'), Atom('tab1'), [(Atom('get'), 'foo', [])], [], 1000)
    res1 = ebf.rpc('gdss', req1)
    assert res1[0] == 'key_not_exist'

    ## add - ok
    req2 = (Atom('do'), Atom('tab1'), [(Atom('add'), 'foo', 1, 'bar', 0, [])], [], 1000)
    res2 = ebf.rpc('gdss', req2)
    assert res2[0] == 'ok'

    ## add - ng
    req3 = (Atom('do'), Atom('tab1'), [(Atom('add'), 'foo', 1, 'bar', 0, [])], [], 1000)
    res3 = ebf.rpc('gdss', req3)
    assert res3[0][0] == 'key_exists'
    assert res3[0][1] == 1

    ## get - ok
    req4 = (Atom('do'), Atom('tab1'), [(Atom('get'), 'foo', [])], [], 1000)
    res4 = ebf.rpc('gdss', req4)
    assert res4[0][0] == 'ok'
    assert res4[0][1] == 1
    assert res4[0][2] == 'bar'

    ## set - ok
    req5 = (Atom('do'), Atom('tab1'), [(Atom('set'), 'foo', 2, 'baz', 0, [])], [], 1000)
    res5 = ebf.rpc('gdss', req5)
    assert res5[0] == 'ok'

    ## get - ok
    req6 = (Atom('do'), Atom('tab1'), [(Atom('get'), 'foo', [])], [], 1000)
    res6 = ebf.rpc('gdss', req6)
    assert res6[0][0] == 'ok'
    assert res6[0][1] == 2
    assert res6[0][2] == 'baz'

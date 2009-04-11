###
### ebf -- A Python-implementation of an EBF client
###
###

import socket, struct, sys, types
from py_interface import erl_term

class SocketError(Exception):
    pass

class EBFError(Exception):
    pass

class Socket:
    def __init__(self,host=socket.gethostname(), port=7476):
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
    def connect(self, host=socket.gethostname(), port=7476, timeout=10):
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

    def login(self, module, host=socket.gethostname(), port=7476, timeout=10):
        self.connect(host, port, timeout)
        self.module = module
        self.timeout = timeout

        # read response - hello
        term = erl_term.BinaryToTerm(self.recv())
        if not self.is_tuple(term, 3):
            raise EBFError, term
        if not self.is_string(term[0], 'ebf1.0'):
            raise EBFError, (term[0], term)
        if not self.is_ubfstring(term[1], 'meta_server'):
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

    def info(self, timeout=None):
        # write request
        if timeout is None:
            self.sock.settimeout(self.timeout)
        else:
            self.sock.settimeout(timeout)
        self.send(erl_term.TermToBinary(self.atom('info')))

        # read response
        term = erl_term.BinaryToTerm(self.recv())
        if not self.is_tuple(term, 2):
            raise EBFError, term

        self.sock.settimeout(self.timeout)
        return term

    def description(self, timeout=None):
        # write request
        if timeout is None:
            self.sock.settimeout(self.timeout)
        else:
            self.sock.settimeout(timeout)
        self.send(erl_term.TermToBinary(self.atom('description')))

        # read response
        term = erl_term.BinaryToTerm(self.recv())
        if not self.is_tuple(term, 2):
            raise EBFError, term

        self.sock.settimeout(self.timeout)
        return term

    def contract(self, timeout=None):
        # write request
        if timeout is None:
            self.sock.settimeout(self.timeout)
        else:
            self.sock.settimeout(timeout)
        self.send(erl_term.TermToBinary(self.atom('contract')))

        # read response
        term = erl_term.BinaryToTerm(self.recv())
        if not self.is_tuple(term, 2):
            raise EBFError, term

        self.sock.settimeout(self.timeout)
        return term

    ### keepalive
    def keepalive(self, timeout=None):
        # write request
        if timeout is None:
            self.sock.settimeout(self.timeout)
        else:
            self.sock.settimeout(timeout)
        self.send(erl_term.TermToBinary(self.atom('keepalive')))

        # read response
        term = erl_term.BinaryToTerm(self.recv())
        if not self.is_tuple(term, 2):
            raise EBFError, term
        if not self.is_atom(term[0], 'ok'):
            raise EBFError, (term[0], term)
        if not self.is_atom(term[1], 'none'):
            raise EBFError, (term[1], term)

        self.sock.settimeout(self.timeout)

    ### rpc
    def rpc(self, module, function, args, maxsize=None, writetimeout=None, readtimeout=None):
        # TODO: implement maxsize

        if not self.module==module:
            raise EBFError, (module, self.module)

        # write request
        if writetimeout is None:
            self.sock.settimeout(self.timeout)
        else:
            self.sock.settimeout(writetimeout)
        self.send(erl_term.TermToBinary(self.tuple([self.atom(function), args])))

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

        self.sock.settimeout(self.timeout)
        return term

if __name__ == "__main__":
    ebf = EBF()

    ebf.login('dque_db_deliveryque')
    ebf.keepalive()
    ebf.keepalive()

    print "info: "
    print repr(ebf.info())

    print "description: "
    print repr(ebf.description())

    print "contract: "
    print repr(ebf.contract())[0:80], "..."

    ebf.keepalive()
    ebf.keepalive()

    try:
        ebf.rpc('dque_db_deliveryque', 'foobar', [])
    except EBFError, msg:
        pass

    try:
        ebf.keepalive()
    except socket.error, msg:
        pass

#!/usr/bin/python

#
#

import json
import urllib2
import time
from pyubf_pyjson import to_pyjson, from_pyjson
from pyubf import Atom, Integer

class JsonResError(RuntimeError):
    pass
class JsonFault(RuntimeError):
    pass

def do_rpc(UserId, method, *params):
    id = getNewId(UserId)

    jsonId = to_pyjson(id)
    jsonMethod = to_pyjson(method)
    jsonParams = [ to_pyjson(param) for param in params ]
    jsonReq = json.write(dict(version='1.1', id=jsonId, method=jsonMethod, params=jsonParams))
    #print jsonReq
    url = 'http://localhost:7590/rpc'
    cookie =  auth_cookie(UserId)
    headers = {'X-temp-auth-info' : cookie }
    req = urllib2.Request(url, jsonReq, headers)
    res = urllib2.urlopen(req)
    jsonRes = json.read(res.read())
    #print jsonRes
    if jsonRes["id"] != jsonId:
        raise JsonResError("Invalid request id (is: %s, expected: %s)" % (jsonRes["id"], jsonId))
    if jsonRes["error"] is not None:
        raise JsonFault("JSON Error", from_pyjson(jsonRes["error"]))
    return from_pyjson(jsonRes["result"])

def do_request(url,UserId, method, *params):
    id = getNewId(UserId)
    url = 'http://localhost:7590/vcard'
    cookie =  auth_cookie(UserId)
    headers = {'X-temp-auth-info' : cookie }
    headers["HOST"]="localhost"
    req = urllib2.Request(url, None, headers)
    res = urllib2.urlopen(req)
    return res.read()

def getNewId(Tag):
    """Build a new Id"""
    t1 = time.time()
    sid = str(Tag) + '_' + str(t1)
    return sid

def auth_cookie(UserId):
    id = 'uraid' + str(UserId)
    strid = str(id)
    return strid[::-1]


if __name__ == "__main__":
    UserId = 1

    # profile get
    res1 = do_rpc(UserId, 'profile_get', Atom('user'), None, Atom('infinity'))
    assert res1[0] == 'ok'
    profile = res1[1]
    ts = res1[2]
    print profile

    # profile set
    profile[1].update({'cos' : Atom('paying')})
    res2 = do_rpc(UserId, 'profile_set', profile, ts, Atom('infinity'))
    assert res2[0] == 'ok'
    assert res2[1] != ts

    # profile get
    res3 = do_rpc(UserId, 'profile_get', Atom('user'), None, Atom('infinity'))
    assert res3[0] == 'ok'
    assert res3[1] == profile
    assert res3[2] == res2[1]

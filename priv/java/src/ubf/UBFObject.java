package ubf;

import java.io.*;

public abstract class UBFObject
{
    public boolean isString()  { return false; }
    public boolean isAtom()    { return false; }
    public boolean isInteger() { return false; }
    public boolean isTuple()   { return false; }
    public boolean isList()    { return false; }
    public boolean isTaggedTuple(String tag, int arity) { return false; }
}


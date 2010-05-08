package ubf;

import java.io.*;

/** Convenience front end. */

public class UBF
{
    public static final boolean DEBUG = false;

    public static UBFObject tuple(UBFObject a) {
	return new UBFTuple(new UBFObject[] { a });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b) {
	return new UBFTuple(new UBFObject[] { a, b });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c) {
	return new UBFTuple(new UBFObject[] { a, b, c });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e, UBFObject f)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e, f });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e, UBFObject f, UBFObject g)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e, f, g });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e, UBFObject f, UBFObject g, UBFObject h)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e, f, g, h });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e, UBFObject f, UBFObject g, UBFObject h, UBFObject i)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e, f, g, h, i });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e, UBFObject f, UBFObject g, UBFObject h, UBFObject i, UBFObject j)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e, f, g, h, i, j });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e, UBFObject f, UBFObject g, UBFObject h, UBFObject i, UBFObject j, UBFObject k)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e, f, g, h, i, j, k });
    }

    public static UBFObject tuple(UBFObject a, UBFObject b, UBFObject c, UBFObject d, UBFObject e, UBFObject f, UBFObject g, UBFObject h, UBFObject i, UBFObject j, UBFObject k, UBFObject l)
    {
	return new UBFTuple(new UBFObject[] { a, b, c, d, e, f, g, h, i, j, k, l });
    }

    public static UBFAtom atom(String s) {
	return new UBFAtom(s);
    }

    /* Networking */

    public static void write(UBFObject object, OutputStream output)
	throws IOException
    {
	new UBFEncoder(output).write(object);
    }

    public static UBFObject read(InputStream input)
	throws IOException, UBFException
    {
	return new UBFDecoder(input).read();
    }

    public static void debug(String msg)
    {
	if (DEBUG)
	    System.err.println(msg);
    }

}


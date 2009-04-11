package ubf;

import java.io.*;

/** Convenience front end. */

public class UBF
{
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

    public static UBFAtom atom(String s) {
	return new UBFAtom(s);
    }

}


package ubf;

import java.io.*;
// import java.util.*;

public class UBFEncoder
{
    protected PrintStream output;

    public UBFEncoder(OutputStream out)
    {
	output = new PrintStream(out);
    }

    public static void writeUBF(OutputStream output, UBFObject object)
	throws IOException
    {
	new UBFEncoder(output).write(object);
    }

    public static String encode(UBFObject object)
	throws IOException
    {
	ByteArrayOutputStream byteout = new ByteArrayOutputStream();
	new UBFEncoder(byteout).write(object);
	return new String(byteout.toByteArray());
    }

    public void write(UBFObject object)
	throws IOException
    {
	writeObject(object);
	output.write('$');
    }

    protected void writeObject(UBFObject object)
	throws IOException
    {
	if (object.isString())
	    writeString((UBFString)object);
	else if (object.isAtom())
	    writeAtom((UBFAtom)object);
	else if (object.isInteger())
	    writeInteger((UBFInteger)object);
	else if (object.isTuple())
	    writeTuple((UBFTuple)object);
	else if (object.isList())
	    writeList((UBFList)object);
	else if (object.isBinary())
	    writeBinary((UBFBinary)object);
	else
	    throw new IllegalArgumentException("Can't determine type of " +
					       object);
    }

    protected void writeString(UBFString s)
	throws IOException
    {
	writeDelimited(s.value, '\"');
    }

    protected void writeAtom(UBFAtom s)
	throws IOException
    {
	writeDelimited(s.value, '\'');
    }

    protected void writeBinary(UBFBinary b)
	throws IOException
    {
	output.print(b.size());
	output.print("~");
	output.write(b.value);
	output.print("~");
    }

    protected void writeDelimited(String s, char delimiter)
	throws IOException
    {
	char[] chars = s.toCharArray();

	output.write(delimiter);
	for (int i = 0; i < chars.length; i++) {
	    if (chars[i] == delimiter) {
		output.write('\\');
		output.write(delimiter);
	    } else if (chars[i] == '\\') {
		output.write('\\');
		output.write('\\');
	    } else {
		output.write((byte)chars[i]);
	    }
	}
	output.write(delimiter);
    }

    protected void writeInteger(UBFInteger obj)
	throws IOException
    {
	output.print(obj.value);
    }

    protected void writeTuple(UBFTuple obj)
	throws IOException
    {
	UBFObject[] elems = obj.value;

	output.print("{");
	for (int i = 0; i < elems.length; i++) {
	    output.print(" ");
	    writeObject(elems[i]);
	}
	output.print(" }");
    }

    protected void writeList(UBFList obj)
	throws IOException
    {
	UBFObject[] array = obj.toArray();
	output.print("#");
	for (int i = array.length-1; i >= 0; i--) {
	    output.print(" ");
	    writeObject(array[i]);
	    output.print(" &");
	}
    }

}


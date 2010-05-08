package ubf;

import java.io.*;
import java.util.*;

public class UBFDecoder
{
    protected PushbackInputStream input;
    protected Stack stack = new Stack();
    protected UBFObject value;
    protected UBFObject register[] = new UBFObject[256];

    public UBFDecoder(InputStream in)
    {
	input = new PushbackInputStream(in);
    }

    public static UBFObject readUBF(InputStream in)
	throws UBFException, IOException
    {
	return new UBFDecoder(in).read();
    }

    public static UBFObject decode(String s)
	throws UBFException, IOException
    {
	ByteArrayInputStream input = new ByteArrayInputStream(s.getBytes());
	return readUBF(input);
    }

    public UBFObject read()
	throws UBFException, IOException
    {
	UBFObject value = null;

	while (value == null) {
	    skipWhitespace();
	    char ch = nextChar();

	    if (ch == '}') {
		return new UBFTuple((UBFObject[])
				    stack.toArray(new UBFObject[0]));
	    }
	    if (ch == '$') {
		if (stack.size() != 1) {
		    throw new UBFException("Can't end with stack size " +
					   stack.size());
		} else {
		    value = (UBFObject)stack.pop();
		}
	    } else {
		input.unread(ch);
		stack.push(readObject());
	    }
	}
	return value;
    }

    protected UBFObject readObject()
	throws UBFException, IOException
    {
	char ch = nextChar();
	input.unread(ch);

	switch (ch) {
	case '#':
	    nextChar();
	    return new UBFList();
	case '&':
	    nextChar();
	    UBFObject a = (UBFObject)stack.pop();
	    UBFObject b = (UBFObject)stack.pop();
	    if (!b.isList())
		throw new UBFException("Can't make cons with cdr " + b);
	    ((UBFList)b).value.addFirst(a);
	    return b;
	case '"':
	    return readString();
	case '\'':
	    return readAtom();
	case '{':
	    nextChar();		//  skip {
	    Stack real_stack = stack;
	    stack = new Stack();
	    UBFObject obj = read();
	    stack = real_stack;
	    if (obj.isTuple())
		return obj;
	    else
		throw new UBFException("Expected tuple: obj");
	case '>':
	    nextChar();
	    char reg = nextChar();
	    register[reg] = (UBFObject)stack.pop();
	    /*
	    ** We don't want to return this object because we don't want to
	    ** have the caller push it on the stack ... so keep going!
	    */
	    return readObject();
	default:
	    if (ch == '-' || Character.isDigit(ch))
		return readInteger();
	    else if (ch == '~') {
		nextChar();
		int len = (int) ((UBFInteger)stack.pop()).value;
		byte buf[] = new byte[len];
		int bytes_read = 0;
		while (bytes_read < len) {
		    bytes_read += input.read(buf, bytes_read, len - bytes_read);
		}
		nextChar();	// Skip trailing ~
		return new UBFBinary(buf, len);
	    }
	    else if (register[ch] != null) {
		nextChar();
		return register[ch];
	    }
	    else
		throw new UBFException("Invalid start character: '" +
				       ch + "' (" + (int)ch + ")");
	}
    }

    protected boolean isWhitespace(char ch)
    {
	switch (ch) {
	case '\n':
	case '\r':
	case '\t':
	case ' ':
	case ',':
	    return true;
	default:
	    return false;
	}
    }

    protected UBFString readString()
	throws UBFException, IOException
    {
	return new UBFString(readDelimitedString());
    }

    protected UBFAtom readAtom()
	throws UBFException, IOException
    {
	return new UBFAtom(readDelimitedString());
    }

    protected String readDelimitedString()
	throws UBFException, IOException
    {
	StringBuffer sb = new StringBuffer();
	char delimiter = nextChar();
	for (char ch = nextChar(); ch != delimiter; ch = nextChar()) {
	    if (ch == '\\')
		sb.append(nextChar());
	    else
		sb.append(ch);
	}
	return sb.toString();
    }

    protected UBFInteger readInteger()
	throws UBFException, IOException
    {
	long acc = 0;
	char ch;
	int sign = 1;

	/* sign check */
	ch = nextChar();
	if (ch == '-') {
	    sign = -1;
	    ch = nextChar();
	    if (!Character.isDigit(ch))
		throw new UBFException("No digits in number, just '-'");
	}

	while (Character.isDigit(ch)) {
	    acc = (acc * 10) + ((ch - '0') * sign);
	    if ((acc > Long.MAX_VALUE) || (acc < Long.MIN_VALUE))
		throw new UBFException("Integer overflow");
	    ch = nextChar();
	}
	input.unread(ch);

	return new UBFInteger((long)acc);
    }

    protected char nextChar()
	throws UBFException, IOException
    {
	int ch = input.read();
	if (ch == -1)
	    throw new UBFException("EOF");
	return (char)ch;
    }

    protected char peekChar()
	throws UBFException, IOException
    {
	int ch = input.read();
	if (ch == -1)
	    throw new UBFException("EOF");
	input.unread(ch);
	return (char)ch;
    }

    /* Drop all leading whitespace input */
    protected void skipWhitespace()
	throws IOException
    {
	char ch;
	do ch = (char)input.read(); while (isWhitespace(ch) || ch == -1);
	if (ch != -1)
	    input.unread(ch);
    }

    /*
     * Testing
     */

    public static void main(String[] args)
	throws Exception
    {
	System.out.println(new UBFDecoder(new ByteArrayInputStream(args[0].getBytes())).read());
								   
    }

}


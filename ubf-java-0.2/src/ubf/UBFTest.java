package ubf;

public class UBFTest
{
    public static void main(String[] args)
	throws Exception
    {
	if (args.length == 0)
	    args = defaultTests();
	for (int i = 0; i < args.length; i++)
	    test(args[i]);
	System.out.println("Finished "+args.length+" tests.");
    }

    /*
     * Simple test of identity: a UBF term is parsed, then
     * unparsed/reparsed, and we make sure that the parse trees are
     * equivalent.
     */
    static void test(String orig)
	throws Exception
    {
	/* simple read/write identity check */
	try {
	    UBFObject parse1 = UBFDecoder.decode(orig);
	    String externed = UBFEncoder.encode(parse1);
	    UBFObject parse2 = UBFDecoder.decode(externed);
	    if (parse1.equals(parse2))
		System.out.println("OK: " + orig + " == " + externed);
	    else
		System.out.println("!!: " + orig + " /= " + externed);
	} catch (Exception e) {
	    System.out.println("Error on " + orig + ":");
	    e.printStackTrace();
	}
    }

    static String[] defaultTests()
    {
	return new String[] {
	    // numbers
	    "1234$",
	    "-4321$",
	    // strings
	    "\"foo\"$",
	    "\"\\\\foo\\\"\\\"\"$",
	    // atoms
	    "'foo'$",
	    "'\\\'foo\\\''$",
	    // tuples
	    "{}$",
	    "{{{}}}$",
	    "{1 \"two\" {3} 'four'}$",
	    // lists
	    "# 1 & 2 & 3 & $",
	    "#$"
	};
    }
}


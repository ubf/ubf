package ubf;

public class UBFAtom extends UBFObject
{
    public final String value;

    public UBFAtom(String s) {
	value = s.intern();
    }

    public boolean isAtom() {
	return true;
    }

    public String toString() {
	return value;
    }

    /**
     * O(1) when argument is an atom, normal string comparison when
     * its a string
     */
    public boolean equals(Object o) {
	return ((o instanceof UBFAtom) && ((UBFAtom)o).value == value)
	    || ((o instanceof String) && o.equals(value));
    }

    public int hashCode() {
	return value.hashCode();
    }
}


package ubf;

public class UBFInteger extends UBFObject
{
    public final long value;

    public UBFInteger(int i) {
	value = (long) i;
    }

    public UBFInteger(long i) {
	value = i;
    }

    public boolean isInteger() {
	return true;
    }

    public String toString() {
	return String.valueOf(value);
    }

    public boolean equals(Object o)
    {
	return (o instanceof UBFInteger) && (((UBFInteger)o).value == value);
    }

    public int hashCode()
    {
	return (int) value;
    }

}

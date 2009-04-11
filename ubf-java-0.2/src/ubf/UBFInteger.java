package ubf;

public class UBFInteger extends UBFObject
{
    public final int value;

    public UBFInteger(int i) {
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
	return value;
    }

}

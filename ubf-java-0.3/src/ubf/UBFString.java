package ubf;

public class UBFString extends UBFObject
{
    public final String value;

    public UBFString(String s) {
	value = s;
    }

    public boolean isString() {
	return true;
    }

    public String toString() {
	return value;
    }

    public boolean equals(Object o) {
	return ((o instanceof UBFString) &&
		(((UBFString)o).value.equals(this.value)));
    }

    public int hashCode() {
	return value.hashCode();
    }
}


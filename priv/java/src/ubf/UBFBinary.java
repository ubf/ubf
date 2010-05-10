package ubf;

public class UBFBinary extends UBFObject
{
    public byte value[];
    public int size;

    public UBFBinary(String s) {
	value = s.getBytes();
	size = value.length;
    }

    public UBFBinary(byte []bytes, int len) {
	value = new byte[len];
	value = bytes;
	size = len;
    }

    public boolean isBinary() {
	return true;
    }

    public String toString() {
	return new String(value);
    }

    public int size() {
	return size;
    }

    public boolean equals(Object o) {
	return ((o instanceof UBFBinary) && ((UBFBinary)o).value == value)
	    || ((o instanceof String) && o.equals(new String(value)));
    }

    public int hashCode() {
	return value.hashCode();
    }
}


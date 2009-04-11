package ubf;

import java.util.*;

public class UBFList extends UBFObject
{
    public final LinkedList value = new LinkedList();

    public UBFList() {
    }

    public UBFList(UBFObject[] arr) {
	for (int i = 0; i < arr.length; i++)
	    value.add(arr[i]);
    }

    public boolean isList() {
	return true;
    }

    public String toString() {
	StringBuffer b = new StringBuffer();
	UBFObject[] value = toArray();
	b.append("[");
	for (int i = 0; i < value.length; i++) {
	    b.append(' ');
	    b.append(value[i].toString());
	}
	b.append(" ]");
	return b.toString();
    }

    public UBFObject[] toArray()
    {
	return (UBFObject[])value.toArray(new UBFObject[0]);
    }

    public boolean equals(Object o) {
	return (o instanceof UBFList) && ((UBFList)o).value.equals(this.value);
    }

}


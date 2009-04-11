package ubf;

import java.util.*;

public class UBFTuple extends UBFObject
{
    public final UBFObject[] value;

    public UBFTuple(UBFObject[] arr) {
	value = arr;
    }

    public boolean isTuple() {
	return true;
    }

    public String toString() {
	StringBuffer b = new StringBuffer();
	b.append("{");
	for (int i = 0; i < value.length; i++) {
	    b.append(' ');
	    b.append(value[i].toString());
	}
	b.append(" }");
	return b.toString();
    }

    public boolean equals(Object o) {
	if (!(o instanceof UBFTuple))
	    return false;

	UBFObject[] elems = ((UBFTuple)o).value;
	if (elems.length != value.length)
	    return false;

	for (int i = 0; i < value.length; i++)
	    if (!(value[i].equals(elems[i])))
		return false;

	return true;
    }

    public boolean isTaggedTuple(String tag, int arity) {
	if ((value.length > 0) && (value.length == arity))
	    return value[0].equals(tag);
	else
	    return false;
    }

}


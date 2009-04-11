package ubf.irc1;

import ubf.*;
import java.io.*;
import java.net.*;

public class IRCClient
{
    protected Socket sk;
    protected InputStream input;
    protected OutputStream output;

    public IRCClient(String nick, String host, int port)
    {
	try {
	    sk = new Socket(host, port);
	    input = sk.getInputStream();
	    output = sk.getOutputStream();
	    login(nick);
	} catch (IOException e) {
	    System.out.println("Connection failed: " + e);
	    System.exit(1);
	} catch (UBFException e) {
	    System.out.println("Couldn't handle UBF:\n");
	    e.printStackTrace();
	} catch (IRCException e) {
	    System.out.println("Error: " + e);
	}
	System.exit(1);
    }

    protected void login(String nick)
	throws IOException, UBFException
    {
	UBF.write(new UBFString("logon"), output);
	UBFObject obj = UBF.read(input);
	if (obj.isTaggedTuple("ok", 2)) {
	    UBF.write(UBF.tuple(new UBFAtom("nick"),
				new UBFString(nick)),
		      output);
	    UBFObject reply = UBF.read(input);
	    if (!reply.equals("nickChanged"))
		throw new IRCException("Unexpected reply to nick change: " +
				       reply);
	} else {
	    throw new IRCException("login failed: " + obj);
	}
    }

    protected void handleEvent(UBFObject ev)
    {
	System.out.println("Event: " + ev);
    }

    public static void main(String[] args)
    {
	if (args.length != 3) {
	    System.err.println("Usage: IRCClient <nick> <host> <port>");
	    System.exit(1);
	}

	new IRCClient(args[0], args[1], Integer.parseInt(args[2]));
    }

}

class IRCException extends RuntimeException
{
    public IRCException(String reason) { super(reason); }
}


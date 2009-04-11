package ubf.irc1;

import ubf.*;
import java.io.*;
import java.net.*;

public class IRCClient implements UBFEventHandler
{
    protected UBFClient client;

    protected Object mutex = new Object();
    protected String currentGroup;

    public IRCClient(String nick, String host, int port)
    {
	try {
	    Socket sk = new Socket(host, port);
	    client = new UBFClient(new UBFString("irc"),
				   new UBFList(),
				   this,
				   sk.getInputStream(),
				   sk.getOutputStream());
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
	new Thread(new KeyboardInputHandler()).start();
    }

    protected void login(String nick)
	throws IOException, UBFException
    {
	UBFObject obj = client.rpc(new UBFAtom("logon"));
	if (obj.isTaggedTuple("ok", 2)) {
	    UBFObject reply = client.rpc(UBF.tuple(new UBFAtom("nick"),
						   new UBFString(nick)));
	    if (!reply.equals("nickChanged"))
		throw new IRCException("Unexpected reply to nick change: " +
				       reply);
	} else {
	    throw new IRCException("login failed: " + obj);
	}
    }

    public void handleEvent(UBFClient cl, UBFObject ev)
    {
	if (ev.isTaggedTuple("msg", 4)) {
	    UBFObject[] msg = ((UBFTuple)ev).value;
	    UBFString nick = (UBFString)msg[1];
	    UBFString group = (UBFString)msg[2];
	    UBFString text = (UBFString)msg[3];
	    System.out.println("[" + group.value + "] " +
			       "<" + nick.value + "> " + text.value);
	} else {
	    System.out.println("Event: " + ev);
	}
    }

    public void connectionClosed(UBFClient client)
    {
	System.err.println("Connection closed!");
	System.exit(1);
    }


    public void handleInput(String input)
	throws IOException, UBFException
    {
	if (input.startsWith("/join ")) {
	    String group = input.substring(input.indexOf(' '));
	    currentGroup = group;
	    client.rpc(UBF.tuple(new UBFAtom("join"), new UBFString(group)));
	} else if (input.startsWith("/leave ")) {
	    String group = input.substring(input.indexOf(' '));
	    client.rpc(UBF.tuple(new UBFAtom("leave"), new UBFString(group)));
	} else {
	    client.rpc(UBF.tuple(new UBFAtom("msg"),
				 new UBFString(currentGroup),
				 new UBFString(input)));
	}
    }

    public static void main(String[] args)
    {
	if (args.length != 3) {
	    System.err.println("Usage: IRCClient <nick> <host> <port>");
	    System.exit(1);
	}

	System.out.println("IRC Client Commands:");
	System.out.println("  /join <channel>");
	System.out.println("  /leave <channel>");
	System.out.println("Or just type a message.");

	new IRCClient(args[0], args[1], Integer.parseInt(args[2]));
    }

    protected class KeyboardInputHandler implements Runnable {
	public void run() {
	    DataInputStream in = new DataInputStream(System.in);
	    try {
		while (true) {
		    String line = in.readLine();
		    if (line == null)
			throw new EOFException();
		    IRCClient.this.handleInput(line);
		}
	    } catch (Exception e) {
		e.printStackTrace();
		System.exit(1);
	    }
	}

    }

}

class IRCException extends RuntimeException
{
    public IRCException(String reason) { super(reason); }
}


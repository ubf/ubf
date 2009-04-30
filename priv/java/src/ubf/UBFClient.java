package ubf;

import java.net.Socket;
import java.io.*;

public class UBFClient
{
    protected InputStream input;
    protected OutputStream output;
    protected UBFEventHandler eventHandler;
    protected UBFAtom serverState;

    // No RPC in progress
    protected static final int RPC_IDLE = 0;
    // Request sent, no reply yet
    protected static final int RPC_PENDING = 1;
    // Reply received in `rpcReply' variable
    protected static final int RPC_DONE = 2;
    // RPC went wrong
    protected static final int RPC_FAILED = 3;

    protected Object mutex = new Object();
    protected int rpcState = RPC_IDLE;
    protected UBFObject rpcReply;

    public UBFClient(UBFString service,
		     UBFList args,
		     UBFEventHandler h,
		     InputStream in,
		     OutputStream out)
	throws UBFException, IOException
    {
	eventHandler = h;
	input = in;
	output = out;
	/* first skip the welcome banner */
	read();
	startEventThread();
	startSession(service, args);
    }

    protected void startSession(UBFString service, UBFList args)
	throws IOException, UBFException
    {
	UBFObject reply = rpc(UBF.tuple(new UBFAtom("startSession"),
					service,
					args));
	if (!reply.isTaggedTuple("ok", 2))
	    throw new UBFException("Unexpected reply to startSession: " +
				   reply);
    }

    public UBFObject rpc(UBFObject request)
	throws IOException, UBFException
    {
	synchronized (mutex) {
	    if (rpcState != RPC_IDLE)
		throw new RuntimeException("Illegal rpcState: " + rpcState);
	    /* make request */
	    rpcState = RPC_PENDING;
	    UBF.write(UBF.tuple(new UBFAtom("rpc"), request), output);
	    UBF.debug("RPC -> " + request);
	    /* wait reply */
	    do {
		try { mutex.wait(); } catch (InterruptedException e) {}
	    } while (rpcState == RPC_PENDING);
	    /* handle result */
	    if (rpcState == RPC_DONE) {
		UBF.debug("RPC <- " + rpcReply);
		UBFObject reply = rpcReply;
		rpcReply = null;
		rpcState = RPC_IDLE;
		return reply;
	    } else if (rpcState == RPC_FAILED)
		throw new UBFException("RPC failed");
	    else
		throw new RuntimeException("Illegal rpcState: " + rpcState);
	}
    }

    protected void startEventThread()
    {
	new Thread(new IOHandler()).start();
    }
    
    public UBFAtom getServerState() { return serverState; }
    
    public InputStream getInputStream()   { return input; }
    public OutputStream getOutputStream() { return output; }
    
    public UBFObject read()
	throws IOException, UBFException
    {
	return UBF.read(input);
    }

    protected class IOHandler implements Runnable
    {
	public void run() {
	    try {
		while (true) {
		    UBFObject msg = UBF.read(input);
		    if (msg.isTaggedTuple("rpcReply", 3)) {
			synchronized (UBFClient.this.mutex) {
			    UBFObject[] replyTerms = ((UBFTuple)msg).value;
			    serverState = (UBFAtom)replyTerms[2];
			    UBFClient.this.rpcReply = (UBFObject)replyTerms[1];
			    UBFClient.this.rpcState = UBFClient.RPC_DONE;
			    mutex.notifyAll();
			}
		    } else if (msg.isTaggedTuple("event", 2)) {
			UBFObject event = ((UBFTuple)msg).value[1];
			UBFClient.this.eventHandler.handleEvent(UBFClient.this, event);
		    }
		}
	    } catch (Exception e) {
		System.err.println("Error in event thread: " + e);
		e.printStackTrace();
	    }
	}
    }
}


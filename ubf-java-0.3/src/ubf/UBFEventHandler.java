package ubf;

public interface UBFEventHandler
{
    public void handleEvent(UBFClient client, UBFObject event);
    public void connectionClosed(UBFClient client);
}


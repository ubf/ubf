#!/bin/sh

if [ $# != 3 ]; then
    echo "Usage: $0 <nick> <server> <port>"
    exit 1
fi

java -cp classes ubf.irc1.IRCClient "$@"


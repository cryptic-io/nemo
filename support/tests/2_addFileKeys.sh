#!/bin/sh

. config.sh

RESERVED_FILE=`cat /tmp/reserved_file_name`
KEY="TESTKEY"

RES=`echo "{\"command\":\"addFileKeys\",\"filekeys\":[{\"filename\":\"$RESERVED_FILE\",\"key\":\"$KEY\"}],\"meta\":{\"http\":true}}" | curl -d@- $HOST:$PRIVATEPORT 2>/dev/null`

SUCCESS=`echo $RES | ./JSON.sh | grep '\["return"\]'`

if [ "$SUCCESS" = "" ]; then
    echo $RES
else
    echo $KEY > /tmp/test_key
fi


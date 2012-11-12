#!/bin/sh

. config.sh

RESERVED_FILE=`cat /tmp/reserved_file_name`
KEY=`cat /tmp/test_key`

RES=`echo "{\"command\":\"downloadFile\",\"filename\":\"$RESERVED_FILE\",\"key\":\"$KEY\",\"meta\":{\"http\":true}}" | curl -d@- $HOST:$PUBLICPORT 2>/dev/null`

if [ "$RES" != "blahblahblah" ]; then
    echo $RES
fi

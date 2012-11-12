#!/bin/sh

. config.sh

RESERVED_FILE=`cat /tmp/reserved_file_name`
FIRST_CHAR=`echo $RESERVED_FILE | awk '{print substr($0,1,1);}'`
SECOND_CHAR=`echo $RESERVED_FILE | awk '{print substr($0,2,1);}'`

FILE_DIR=$FILE_LOCATION$FIRST_CHAR/$SECOND_CHAR/
FILE_NAME=$FILE_DIR$RESERVED_FILE

mkdir -p $FILE_DIR
echo "blahblahblah" > $FILE_NAME

RES=`echo "{\"command\":\"addFile\",\"filename\":\"$RESERVED_FILE\",\"meta\":{\"http\":true}}" | \
    curl -d@- $HOST:$PRIVATEPORT 2>/dev/null`

SUCCESS=`echo $RES | ./JSON.sh | grep '\["success"\]'`

if [ "$SUCCESS" = "" ]; then
    echo $RES
fi

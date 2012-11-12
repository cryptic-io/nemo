#!/bin/sh

. config.sh

echo '{"command":"reserveFile","meta":{"http":true}}' | curl -d@- $HOST:$PRIVATEPORT 2>/dev/null | \
    ./JSON.sh | grep '\["success"\]' | awk '{print $2}' | grep -oP '[^"]+' > /tmp/reserved_file_name

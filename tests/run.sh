#!/bin/sh

#cd into directory which houses this script (/test)
cd $( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

FILES=`ls | grep -P '^[0-9]' | sort`

for file in $FILES; do
    echo "Running $file"
    R=`./$file`
    if [ "$R" != "" ]; then
        echo "ERROR"
        echo $R
        exit 1
    fi
done


#!/bin/bash
###

# Tests cover solutions for the following Issues:
#   79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 91, 92, 93, 94, 95, 96, 99,
#   100, 105, 106
###

PLATFORM=${1:-unix}

echo "Selected Platform: ${PLATFORM}"
echo

for testcase in `find . -name "*.cls"`
do
    answer=$(head -1 $testcase)
    if [ "$PLATFORM" = "windows" ]
    then
        answer=$(echo ${answer#*: } | sed -e 's/\\n/\r\n/g')
    elif [ "$PLATFORM" = "mac" ]
    then
        answer=$(echo ${answer#*: } | sed -e 's/\\n/\r\n/g')
    else
        answer=$(echo ${answer#*: } | sed -e 's/\\n/\n/g')
    fi

    printf "%-24s" "${testcase#*/}:"

    result=`echo 10 | CLASSES/Interp "$testcase"`

    if [ "$result" = "$answer" ]
    then
        outcome="success."
    else
        outcome="FAILURE!"
    fi
    printf "%s\n" "$outcome"
done




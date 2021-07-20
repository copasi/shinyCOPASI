#!/usr/bin/env bash

# This is largely just to give an example of how to start and 
# stop singularity container instances

INSTANCE_NAME=shiny_copasi

print_usage() {
    printf "
    Start, or stop a COPASI Disease Library web application
    as a Singularity service instance.
    (See https://singularity.hpcng.org/user-docs/3.7/running_services.html)

    Usage: $0 [start|stop]

    With no arguments, or 'start', start the image instance.
    'stop' will . . . well . . . you know . . .

"
}

if [ -z $1 ] || [ "$1" = "start" ]
then
    ACTION=starting
    singularity instance start --writable-tmpfs ShinyCOPASI.sif $INSTANCE_NAME

    echo still need to make the image report that it is serving the app on . . .
    echo "localhost:4285"
elif [ "$1" = "stop" ]
then
    ACTION=stopping
    singularity instance stop $INSTANCE_NAME
else
    print_usage
    exit 1
fi

printf "$ACTION $INSTANCE_NAME singularity instance\n"

singularity instance list

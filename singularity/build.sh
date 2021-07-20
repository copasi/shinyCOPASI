#!/usr/bin/env bash

SINGULARITY=${SINGULARITY:-"$(which singularity)"}
DEF=build.def
OUTNAME=ShinyCOPASI

# can use sandbox (build in a directory, vs. sif file)
# to enable testing addition of dependencies, without 
# having to rebuild everything, every time (e.g. R
# recompiles for every build) until it works. I.e.
# install dependencies by shelling into writeable
# sandbox until things work.
# Make sure to add successful install lines to 
# %post section of def file, as you go,
# for final build to a sif file!!!
SNDBX=

print_usage() {
    printf "
    Usage: $0 [-s] [-b] [-h]
    -s    write to sandbox instead of sif file
    -b    backup previously create sif, if exists
    -h    print this help message and exit
\n"
}

backup_sif() {
    SIF_NAME=$OUTNAME.sif
    printf "backup $SIF_NAME\n"
    if [ -e $SIF_NAME ]
    then
        mv -v $SIF_NAME "${OUTNAME}_$(date -Iseconds).sif"
    else
        printf "$SIF_NAME does not exist\n"
    fi
}

while getopts 'sbh' flag; do
  case "${flag}" in
    s) 
        SNDBX="--sandbox"
	printf "build in sandbox directory ${OUTNAME}\n"
	;;
    b) 
	backup_sif
	;;
    h)
	print_usage
	exit 1
	;;
    \?)
        echo "Unknown option: -$OPTARG" >&2
	print_usage
	exit 1
	;;
  esac
done

if [ -z $SNDBX ]
then
    OUTNAME=$OUTNAME.sif
    printf "build ${OUTNAME}\n"
fi

sudo "${SINGULARITY}" build $SNDBX --force $OUTNAME $DEF 2>&1 | tee build.log

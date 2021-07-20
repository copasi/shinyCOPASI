#!/usr/bin/env bash

# This is largely to provide an example of a way to 
# develop and test the web app code in coordination
# with the singularity image spec.
# The code will not work until a "build --sandbox"
# is first run.

# The idea is to make sure things work with a singularity
# sandbox built (from a/the recent def file).

# The --bind call will allow modification/development of 
# the code in the actual web app library, because it
# stacks over (masks) the web app code built into
# the actual image (sandbox directory, in this case).

# Using a --writable sandbox allows any necessary
# modifications (to dependencies, etc.) to the
# singularity environment.

# But it will be IMPORTANT to make sure any
# working changes are added to the singularity
# definition (def) file for the production
# build of the sif file. A great practice would
# be to have the def file open in a nearby editor,
# and then add each modification immediately after it
# has been added to the sandbox (remove from the 
# def, if the change ends up not working out).

cd ../
sudo singularity shell\
    --writable\
    --bind ./:/shinyCOPASI\
    singularity/ShinyCOPASI

#!/bin/bash

# A "set" of orbitees. If they're present, the value is 1, if not it's the empty string
# test whether something is there using
# if [ "${orbits[ASD]}" ] ; then echo World ; fi
declare -A current_orbitees
# In the first step, we shall consider all satellites of COM
current_orbitees["COM"]=1
# adding and deleting a key
current_orbitees["ASD"]=""
unset current_orbitees["ASD"]

declare -A next_orbitees
# an empty string means it does not exist. But it does show up in the length entry.
next_orbitees["ASD"]=""
unset next_orbitees["ASD"]

orbits_counter=0
step=1

$INP="ASD)FGH\nCOM)ASD"
# alternatively
# $INP=`cat input1.txt`
# Store input in an array, line by line
# https://stackoverflow.com/a/918931/2550406
IFS='\n' read -ra IN <<< "$INP"

# generator that loops the input
linenr=-1
function next_inp_line {
    linenr=$(( ( 1 + linenr ) % ${#current_orbitee[@]} ))
    echo ${IN[$linenr]}
}

# while current_orbitee_set_size greater 0
while [ ${#current_orbitee[@]} -gt 0 ]
do
    local num_satellites=0
    unset next_orbitees
    declare -A next_orbitees

    # make sure we get the line N times before we check again whether the set is empty
    # get next line
    local line="$(next_inp_line)"
    # split the line into orbitee and satellite
    IFS=';' read -ra ORBIT <<< "$line"

    

    orbits_counter=$(( orbits_counter + num_satellites*step ))
    step=$(( step + 1 ))
    current_orbitees=$next_orbitees
done

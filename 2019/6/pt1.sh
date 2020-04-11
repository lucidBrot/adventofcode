#!/bin/bash
set -eux

#magic imported from stackoverflow
#https://stackoverflow.com/a/52651361/2550406
### Function vcp    -VariableCoPy-  
# $1 Name of existing Source-Variable  
# $2 Name for the Copy-Target  
vcp() {
    local var=$(declare -p $1)
    var=${var/declare /declare -g }
    eval "${var/$1=/$2=}"
}


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

INP=$'ASD)FGH\nCOM)ASD'
# alternatively
# $INP=`cat input1.txt`
# Store input in an array, line by line
# https://stackoverflow.com/a/918931/2550406
readarray -O0 -d $"\n" -t IN <<< "$INP"
echo "${IN[@]}"

# while current_orbitee_set_size greater 0
while [ ${#current_orbitees[@]} -gt 0 ]
do
    num_satellites=0
    unset next_orbitees
    declare -A next_orbitees

    # For all input lines
    for line in "${IN[@]}"
    do
        # split the line into orbitee and satellite
        IFS=')' read -ra ORBIT <<< "$line"
        # TODO: skip duplicate lines somehow
        orbitee=${ORBIT[0]}
        if [ "${current_orbitees[$orbitee]}" ]
        then
            next_orbitees["$orbitee"]=1
            num_satellites=$(( 1 + num_satellites ))
        fi
    done

    orbits_counter=$(( orbits_counter + num_satellites*step ))
    step=$(( step + 1 ))
    #current_orbitees=("${next_orbitees[@]}")
    vcp next_orbitees current_orbitees
done

echo $orbits_counter

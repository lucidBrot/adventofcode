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
unset current_orbitees["ASD"]

orbits_counter=0
step=1

current_orbitee_set_size=${#current_orbitees[@]}

echo $current_orbitee_set_size

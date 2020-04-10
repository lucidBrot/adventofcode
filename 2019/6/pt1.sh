#!/bin/bash

# A "set" of orbitees. If they're present, the value is 1
declare -A orbits
orbits["ASD"]=1
echo ${orbits[GHJ]}
echo ${orbits["ASD"]}
if [ "${orbits[GHJ]}" ] ; then echo hello ; fi
if [ "${orbits[ASD]}" ] ; then echo World ; fi

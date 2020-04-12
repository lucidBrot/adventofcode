#!/bin/bash
set -ex
step=0
declare -A santas_orbitees
santas_orbitees["SAN"]=0 # not really needed
declare -A your_orbitees
your_orbitees["YOU"]=0 # or perhaps it is needed to have the array not seem unbound

# --- FUNCTIONS ---

entries_matching () {
    latest_santa="$1"
    latest_you="$2"
    if [[ ${your_orbitees[$latest_santa]} ]]
    then
        echo "$latest_santa"
    fi
    if [[ ${santas_orbitees[$latest_you]} ]]
    then
        echo "$latest_you"
    fi
    echo ""
}

# --- MAIN ---

# This is effectively a do-while loop
match="a"
# Start with SAN and YOU as previous values
santas_orbitee='SAN'
your_orbitee='YOU'
while
    # do everything. TODO: only do that if not COM
    santas_orbitee="$(grep ')'$santas_orbitee input2.txt | sed -e 's/).*$//g')"
    your_orbitee="$(grep ')'$your_orbitee input2.txt | sed -e 's/).*$//g')"
    step=$((1 + step))
    # store the step number for easy retrieval later
    santas_orbitees[$santas_orbitee]=$step
    your_orbitees["$your_orbitee"]=$step

    match="$(entries_matching "$santas_orbitee" "$your_orbitee")"
    empty=''

    # as long as this check holds
    (( "match" == empty ))
do
    continue
done

# subtract the steps SAN->A, YOU->B and the duplicate count of the intersection
num_steps_needed=$(( ${santas_orbitees[$match]} + ${your_orbitees[$match]} - 3 ))
echo "$num_steps_needed"

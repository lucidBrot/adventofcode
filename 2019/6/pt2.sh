#!/bin/bash
set -eux
step=0
declare -A santas_orbitees
declare -A your_orbitees

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
while
    # do everything
    santas_orbitee="$(grep ')SAN$' input1.txt | sed -e 's/).*$//g')"
    your_orbitee="$(grep ')YOU$' input1.txt | sed -e 's/).*$//g')"
    step=$((1 + step))
    # store the step number for easy retrieval later
    santas_orbitees[$santas_orbitee]=$step
    your_orbitees[$your_orbitee]=$step

    match=entries_matching santas_orbitee your_orbitee

    # as long as this check holds
    [[ $match != "" ]] # check here
do
    continue
done

# subtract the steps SAN->A, YOU->B and the duplicate count of the intersection
num_steps_needed = $(( ${santas_orbitees[$match]} + ${your_orbitees[$match]} - 3 ))
echo "$num_steps_needed"

#!/bin/bash
## RENAME FOR YOUR JOB
#PBS -N wmchad-run-get-networks-2011-07-c

## EDIT FOR YOUR JOB
## Request 8 CPUs (cores) on 2 nodes, 16 total cores
#PBS -l nodes=1:ppn=4,mem=20gb,feature=16core

## WALLTIME DEFAULTS TO ONE HOUR - ALWAYS SPECIFY FOR LONGER JOBS
## If the job doesn't finish in 30 minutes, cancel it
#PBS -l walltime=06:00:00
#PBS -M wmchad@u.washington.edu
#PBS -m abe

## EDIT FOR YOUR JOB
## Put the output from jobs into the below directory
#PBS -o /ischool/jb/roshan_anon/results/wmchad/output
## Put both the stderr and stdout into a single file
#PBS -j oe

## EDIT FOR YOUR JOB
## Specify the working directory for this job
#PBS -d /ischool/jb/roshan_anon/wmchad/Code/Shell/Common/DataParsing

# If you can't run as many tasks as there are cores due to memory constraints
# you can simply set HYAK_SLOTS to a number instead.
HYAK_SLOTS=1
# HYAK_SLOTS=`wc -l < $PBS_NODEFILE`

# Prevent tasks from exceeding the total RAM of the node
# Requires HYAK_SLOTS to be set to number of tasks started.
NODEMEM=`grep MemTotal /proc/meminfo | awk '{print $2}'`
NODEFREE=$((NODEMEM-2097152))
MEMPERTASK=$((NODEFREE/2))
ulimit -v $MEMPERTASK

for d in 21 22 23 24 25 26 27 28 29 30 31
do
    ./PairedData-GetMultipleNetworks.sh 11 07 $d 06 00 00 19 59 59 1800
done

exit 0

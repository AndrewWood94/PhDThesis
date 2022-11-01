#!/bin/sh

echo "Running on Eddie..."

WORKINGDIR=/home/s1777204/GPX_Eddie

EXE=$WORKINGDIR/execute_Hikr.sh

START=0
END=3

name="GET_TERRAIN_RUN"

#turn into executable
chmod +x $EXE

# set runtime limit
##-l h_rt=01:00:00
#choose number of cores
## -pe sharedmem 4 -R y
# set memory limit per core
## -l h_vmem=4G

CORES=2

#For number of jobs to submit
for i in `seq $START $END`
do
    qsub -pe sharedmem $CORES -R y -N $name -l h_rt=04:00:00 -l h_vmem=4G -v EN=$i -v CORES=$CORES $EXE
done

echo "$0 done!"

exit 0
    

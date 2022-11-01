#!/bin/sh
#$ -e /exports/eddie/scratch/s1777204
#$ -o /exports/eddie/scratch/s1777204

SUBDIR=$JOB_ID
echo "SUBDIR is $SUBDIR"

END=$EN
CORECOUNT=$CORES

EXECDIR=/home/s1777204/GPX_Eddie

# initiallise environment module
. /etc/profile.d/modules.sh

module load anaconda
source activate GPX_Eddie

time python -u $EXECDIR/scripts/rerunR -c $EXECDIR/configHikr.yaml -l listfile_$END.txt -cc $CORECOUNT >> /exports/eddie/scratch/s1777204/logHikr.txt

echo "$0 done!"

exit 0

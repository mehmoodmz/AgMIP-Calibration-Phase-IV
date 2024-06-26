#!/bin/bash
#SBATCH -p batch
#SBATCH -t 120:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --mail-user=mzeeshan.mehmood@okstate.edu
#SBATCH --mail-type=end

module load R/4.3.1

TDIR=$(mktemp -d)
pushd ${TDIR}

cp -r /home/zeeshan/DSSAT48 ${TDIR}

TDIR=${TDIR} R -f /home/zeeshan/AgMIP-Calibration-Phase-IV/main_script_CERES.R

popd
rm -rf ${TDIR}

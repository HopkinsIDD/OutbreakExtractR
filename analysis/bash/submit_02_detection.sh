#!/bin/bash
# submit_02_detection.sh  —  Batch 2: outbreak detection (per country)
#
# Each array task processes one YAML config from analysis/configs/detection_set/.
# One config = one country; the script globs all Stage 1 flat parquet files for
# that country and runs identify_outbreaks() + trigger_alert() per time window.
#
# Typically submitted with a dependency on Batch 1:
#   BATCH1=$(sbatch --parsable analysis/bash/submit_01_pull_data.sh)
#   sbatch --dependency=afterok:$BATCH1 analysis/bash/submit_02_detection.sh
#
# Can also be submitted independently (if Stage 1 outputs already exist).

#SBATCH --job-name=cholera_detect
#SBATCH --output=logs/%x_%A_%a.log
#SBATCH --error=logs/%x_%A_%a.log
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --time=01:00:00
#SBATCH --export=ALL
#SBATCH --partition=shared-cpu
# EDIT: set upper bound to (N detection_set configs - 1)
# The exact value is printed by 00_make_configs.R
#SBATCH --array=0-8%10

module load GCCcore/12.3.0 GCC/12.3.0 libdeflate/1.18 Abseil/20230125.3 OpenMPI/4.1.5 R/4.3.2  GDAL/3.7.1

# Set taxonomy credentials
source analysis/bash/set_taxonomy_api_key.sh


echo "===== Batch 2 start: $(date) ====="
echo "SLURM_JOB_ID:        $SLURM_JOB_ID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
echo "Hostname:            $(hostname)"

# --- R executable ---
if command -v module &>/dev/null; then
  module load R 2>/dev/null || true
fi
RSCRIPT=$(command -v Rscript)
echo "Rscript: $RSCRIPT"

# --- Config selection ---
CONFIGDIR=analysis/configs/detection_set

if [ ! -d "$CONFIGDIR" ]; then
  echo "ERROR: Config directory not found: $CONFIGDIR"
  echo "Run: Rscript analysis/00_make_configs.R"
  exit 1
fi

CONFIGNAMES=($(ls "$CONFIGDIR" | sort -V))
N_CONFIGS=${#CONFIGNAMES[@]}
echo "Config set: $CONFIGDIR ($N_CONFIGS configs)"

if [ "$SLURM_ARRAY_TASK_ID" -ge "$N_CONFIGS" ]; then
  echo "Task ID $SLURM_ARRAY_TASK_ID >= N_CONFIGS $N_CONFIGS — nothing to do."
  exit 0
fi

THISCONFIG="$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}"
echo "Config: $THISCONFIG"

# --- Run Stage 2 ---
$RSCRIPT analysis/02_run_outbreak_detection.R -c "$THISCONFIG" --redo FALSE || {
  echo "ERROR: 02_run_outbreak_detection.R failed for $THISCONFIG"
  exit 1
}

echo "===== Batch 2 end: $(date) ====="

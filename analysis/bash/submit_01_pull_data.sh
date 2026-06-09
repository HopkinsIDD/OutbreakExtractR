#!/bin/bash
# submit_01_pull_data.sh  —  Batch 1: pull + normalize data (country × time window)
#
# Each array task processes one YAML config from analysis/configs/pull_set/.
# One config = one country × one time window.
#
# Prerequisites:
#   1. Generate configs:
#        Rscript analysis/00_make_configs.R
#   2. Set credentials (export persists to child SLURM jobs via --export=ALL):
#        export CHOLERA_API_USERNAME=<your_username>
#        export CHOLERA_API_KEY=<your_api_key>
#   3. Create log directory:
#        mkdir -p logs
#
# Submission:
#   BATCH1=$(sbatch --parsable analysis/bash/submit_01_pull_data.sh)
#   echo "Batch 1 job ID: $BATCH1"
#
# Then chain Batch 2:
#   sbatch --dependency=afterok:$BATCH1 analysis/bash/submit_02_detection.sh
#
# To re-run specific failed tasks (e.g. tasks 3 and 7):
#   sbatch --array=3,7 analysis/bash/submit_01_pull_data.sh

#SBATCH --job-name=cholera_pull
#SBATCH --output=logs/%x_%A_%a.log
#SBATCH --error=logs/%x_%A_%a.log
#SBATCH --mem=8G
#SBATCH --cpus-per-task=2
#SBATCH --time=02:00:00
#SBATCH --export=ALL
# Yggdrasil partition — verify available partitions with: sinfo -s
# Common options: shared-cpu, cpu, bigmem
#SBATCH --partition=shared-cpu
# EDIT: set upper bound to (N pull_set configs - 1)
# The exact value is printed by 00_make_configs.R
#SBATCH --array=0-35%25

echo "===== Batch 1 start: $(date) ====="
echo "SLURM_JOB_ID:        $SLURM_JOB_ID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
echo "Hostname:            $(hostname)"

# --- R executable ---
# Yggdrasil uses Lmod; check available versions with: module spider R
if command -v module &>/dev/null; then
  module load R 2>/dev/null || true
fi
RSCRIPT=$(command -v Rscript)
echo "Rscript: $RSCRIPT"
$RSCRIPT --version

# --- Validate credentials ---
if [ -z "$CHOLERA_API_USERNAME" ] || [ -z "$CHOLERA_API_KEY" ]; then
  echo "ERROR: CHOLERA_API_USERNAME and CHOLERA_API_KEY must be set."
  echo "Run: export CHOLERA_API_USERNAME=<user> CHOLERA_API_KEY=<key>"
  exit 1
fi

# --- Config selection ---
CONFIGDIR=analysis/configs/pull_set

if [ ! -d "$CONFIGDIR" ]; then
  echo "ERROR: Config directory not found: $CONFIGDIR"
  echo "Run: Rscript analysis/00_make_configs.R"
  exit 1
fi

# sort -V ensures natural (numeric) ordering: pull_set_1, pull_set_2, ..., pull_set_10
CONFIGNAMES=($(ls "$CONFIGDIR" | sort -V))
N_CONFIGS=${#CONFIGNAMES[@]}
echo "Config set: $CONFIGDIR ($N_CONFIGS configs)"

if [ "$SLURM_ARRAY_TASK_ID" -ge "$N_CONFIGS" ]; then
  echo "Task ID $SLURM_ARRAY_TASK_ID >= N_CONFIGS $N_CONFIGS — nothing to do."
  exit 0
fi

THISCONFIG="$CONFIGDIR/${CONFIGNAMES[$SLURM_ARRAY_TASK_ID]}"
echo "Config: $THISCONFIG"

# --- Run Stage 1 ---
$RSCRIPT analysis/01_pull_data.R -c "$THISCONFIG" --redo FALSE || {
  echo "ERROR: 01_pull_data.R failed for $THISCONFIG"
  exit 1
}

echo "===== Batch 1 end: $(date) ====="

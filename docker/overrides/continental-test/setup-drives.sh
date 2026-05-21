#!/bin/bash
# Local-docker override of continental-test/setup-drives.sh.
#
# The production script does `sudo rm -rf /mnt; sudo mkfs.xfs $DRIVE; sudo mount`
# and creates a 16 GB swapfile. Running that against a docker-managed `/mnt`
# bind-mounted from a named volume would wipe a user's previous run's outputs,
# and `swapon` doesn't work in unprivileged containers.
#
# In the local setup `/mnt` is already a writable shared volume. All we need
# is to ensure the subdirectories model-parameters.sh referenced (PNGDIR,
# OUTPUTDIR, TILEDIR) exist.
set -e
mkdir -p "$PNGDIR" "$OUTPUTDIR" "$TILEDIR"
echo "[local-override] setup-drives: ensured $PNGDIR $OUTPUTDIR $TILEDIR"

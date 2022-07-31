#!/bin/bash

# Parse input arguments
PROJECT_DIR=$1
REMOTE_DIR=$2
REMOTE_HOME=$3
IMAGE_FILE=$4

# Knit the document
singularity exec \
    --bind "$PROJECT_DIR":"$REMOTE_DIR" \
    --cleanenv \
    --home "$REMOTE_HOME" \
    --no-home \
    --pwd "$REMOTE_DIR" \
    "$IMAGE_FILE" \
    Rscript -e "rmarkdown::render(input = 'manuscript.Rmd')"

# User-defined variables
MAIN_CMD		:= Rscript -e "rmarkdown::render(input = 'manuscript.Rmd')"
IMAGE_USER		:= alexenge
IMAGE_NAME      := aha
IMAGE_TAG	    := latest

# Automatic workflow variables
PROJECT_DIR		:= $(CURDIR)
PROJECT_NAME	:= $(notdir $(CURDIR))
IMAGE_URL       := docker://$(IMAGE_USER)/$(IMAGE_NAME):$(IMAGE_TAG)
IMAGE_FILE      := $(PROJECT_DIR)/$(IMAGE_NAME)_$(IMAGE_TAG).sif
REMOTE_DIR		:= /home/rstudio/project
REMOTE_HOME     := /home/rstudio
SHELL			:= bash

# Main command
all:
	$(MAIN_CMD)

# Main command with Docker
docker:
	docker run --rm --volume $(PROJECT_DIR):$(REMOTE_DIR) $(IMAGE_TAG) \
	$(MAIN_CMD)

# Main command via SLURM and Singularity on an HPC cluster
sbatch:
	sbatch --chdir $(PROJECT_DIR) --cpus-per-task 40 \
	--mem 180G --nodes 1 --ntasks 1 --time 24:00:00 \
	run_slurm.sh $(PROJECT_DIR) $(REMOTE_DIR) $(REMOTE_HOME) $(IMAGE_FILE)
srun:
	srun --chdir $(PROJECT_DIR) --cpus-per-task 1 \
	--mem 4G --nodes 1 --ntasks 1 --time 01:00:00 \
	run_slurm.sh $(PROJECT_DIR) $(REMOTE_DIR) $(REMOTE_HOME) $(IMAGE_FILE)

# Run an interactive RStudio session with Docker
interactive:
	docker run --rm --volume $(PROJECT_DIR):$(REMOTE_DIR) \
	-e PASSWORD=1234 -p 8888:8888 $(IMAGE_TAG)

# Build the container with Docker
build: Dockerfile
	docker build --no-cache --progress plain --tag $(IMAGE_TAG) .

# Push the container with Docker
push:
	docker push $(IMAGE_TAG)

# Pull the container with Docker
pull:
	docker pull $(IMAGE_TAG)

# Pull the container with Singularity
pull_singularity:
	singularity pull --disable-cache --force $(IMAGE_URL)

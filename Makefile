# User-defined variables
DOCKER_USER		:= alexenge
IMAGE_VERSION	:= main
MAIN_CMD		:= Rscript -e "rmarkdown::render(input = 'manuscript.Rmd')"

# Automatic workflow variables
PROJECT_DIR		:= $(CURDIR)
PROJECT_NAME	:= $(notdir $(CURDIR))
IMAGE_TAG 		:= $(DOCKER_USER)/$(PROJECT_NAME)
REMOTE_DIR		:= /home/rstudio/project
SHELL			:= bash

# Main command
all:
	$(MAIN_CMD)

# Main command with Docker
docker:
	docker run --rm --volume $(PROJECT_DIR):$(REMOTE_DIR) $(IMAGE_TAG) \
	$(MAIN_CMD)

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

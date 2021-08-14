# Set some local variables
SHELL := bash
DOCKER_USER := alexenge
PROJECT := $(notdir $(CURDIR))
HOST_PATH := $(CURDIR)
CONTAINER_PATH := /home/rstudio/aha
CONTAINER_TAG := $(DOCKER_USER)/$(PROJECT)

# If DOCKER=TRUE, do stuff inside in the container
ifeq ($(DOCKER),TRUE)
	run := docker run \
		--rm \
		--volume $(HOST_PATH)/manuscript:$(CONTAINER_PATH)/manuscript \
		--volume $(HOST_PATH)/data:$(CONTAINER_PATH)/data \
		$(CONTAINER_TAG)
	workdir := $(CONTAINER_PATH)
else
	workdir := $(HOST_PATH)
endif

# Knit the manuscript
all: manuscript/manuscript.pdf
manuscript/manuscript.pdf: manuscript/manuscript.Rmd
manuscript/manuscript.pdf: manuscript/apa.csl
manuscript/manuscript.pdf: manuscript/references.bib
manuscript/manuscript.pdf: manuscript/r-references.bib
manuscript/manuscript.pdf: manuscript/example_stim.png
manuscript/manuscript.pdf:
	$(run) Rscript -e "rmarkdown::render(input = '$(workdir)/manuscript/manuscript.Rmd')"

# Build the docker container
build: Dockerfile
	docker build --tag $(CONTAINER_TAG) .

# Save the docker image
save: $(PROJECT).tar.gz
$(PROJECT).tar.gz:
	docker save $(PROJECT):latest | gzip > $@

# Run an interactive RStudio session in the container
interactive:
	docker run \
		--rm \
		--volume $(HOST_PATH)/manuscript:$(CONTAINER_PATH)/manuscript \
		--volume $(HOST_PATH)/data:$(CONTAINER_PATH)/data \
		-e PASSWORD=1234 \
		-p 8888:8888 \
		$(CONTAINER_TAG)

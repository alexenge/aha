# Set some local variables
SHELL := bash
DOCKER_USER := alexenge
PROJECT := $(notdir $(CURDIR))
HOST_PATH := $(CURDIR)
CONTAINER_PATH := /home/rstudio
MEMORY := 14g
CPUS := 7

# If DOCKER=TRUE, do stuff inside in the container
ifeq ($(DOCKER),TRUE)
	run := docker run \
		--rm \
		--memory=${MEMORY} \
		--cpus=${CPUS} \
		--volume $(HOST_PATH)/analysis:$(CONTAINER_PATH)/analysis \
		--volume $(HOST_PATH)/data:$(CONTAINER_PATH)/data \
		$(PROJECT)
	workdir := $(CONTAINER_PATH)
else
	workdir := $(HOST_PATH)
endif

# Knit the manuscript
all: analysis/manuscript.pdf
analysis/manuscript.pdf: analysis/manuscript.Rmd
analysis/manuscript.pdf: analysis/manuscript_files/apa.csl
analysis/manuscript.pdf: analysis/manuscript_files/r-references.bib
analysis/manuscript.pdf: analysis/manuscript_files/potato_masher.png
analysis/manuscript.pdf:
	$(run) Rscript -e "rmarkdown::render(input = '$(workdir)/analysis/manuscript.Rmd')"

# Build the docker container
build: Dockerfile
	docker build --tag $(DOCKER_USER)/$(PROJECT) .

# Save the docker image
save: $(PROJECT).tar.gz
$(PROJECT).tar.gz:
	docker save $(PROJECT):latest | gzip > $@

# Run an interactive RStudio session in the container
interactive:
	docker run \
		--rm \
		--memory=${MEMORY} \
		--cpus=${CPUS} \
		--volume $(HOST_PATH)/analysis:$(CONTAINER_PATH)/analysis \
		--volume $(HOST_PATH)/data:$(CONTAINER_PATH)/data \
		-it \
		-e PASSWORD=1234 \
		-p 8888:8888 \
		$(PROJECT)

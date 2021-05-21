# Set some local variables
SHELL := bash
DOCKER_USER := alexenge
PROJECT := $(notdir $(CURDIR))
HOST_PATH := $(CURDIR)
CONTAINER_PATH := /home/rstudio
CONTAINER_TAG := $(DOCKER_USER)/$(PROJECT)

# If DOCKER=TRUE, do stuff inside in the container
ifeq ($(DOCKER),TRUE)
	run := docker run \
		--rm \
		--volume $(HOST_PATH)/code:$(CONTAINER_PATH)/code \
		--volume $(HOST_PATH)/data:$(CONTAINER_PATH)/data \
		$(CONTAINER_TAG)
	workdir := $(CONTAINER_PATH)
else
	workdir := $(HOST_PATH)
endif

# Knit the manuscript
all: code/manuscript.pdf
code/manuscript.pdf: code/manuscript.Rmd
code/manuscript.pdf: misc/apa.csl
code/manuscript.pdf: misc/bibliography.bib
code/manuscript.pdf: misc/potato_masher.png
code/manuscript.pdf:
	$(run) Rscript -e "rmarkdown::render(input = '$(workdir)/code/manuscript.Rmd')"

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
		--volume $(HOST_PATH)/code:$(CONTAINER_PATH)/code \
		--volume $(HOST_PATH)/data:$(CONTAINER_PATH)/data \
		-e PASSWORD=1234 \
		-p 8888:8888 \
		$(CONTAINER_TAG)

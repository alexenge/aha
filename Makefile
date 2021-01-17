# Set some local variables
SHELL := bash
PROJECT := $(notdir $(CURDIR))
HOST_PATH := $(CURDIR)
CONTAINER_PATH := /home/rstudio
MEMORY := 14g
CPUS := 7

# If DOCKER=TRUE, we want to knit the document via Docker, else locally
ifeq ($(DOCKER),TRUE)
	run := docker run --rm --memory=${MEMORY} --cpus=${CPUS} \
	--volume $(HOST_PATH)/analysis:$(CONTAINER_PATH)/analysis $(PROJECT)
	workdir := $(CONTAINER_PATH)
else
	workdir := $(HOST_PATH)
endif

# The default target is the manuscript
all: analysis/manuscript.pdf

# Another target is to build the Docker container
build: Dockerfile
	docker build --tag $(PROJECT) .

# Another target is to save the Docker image
save: $(PROJECT).tar.gz
$(PROJECT).tar.gz:
	docker save $(PROJECT):latest | gzip > $@

# Another target is to run an interactive RStudio session in the container
interactive:
	docker run --rm -it --memory=${MEMORY} --cpus=${CPUS} -e PASSWORD=1234 -p 8787:8787 \
	--volume $(HOST_PATH)/analysis:$(CONTAINER_PATH)/analysis $(PROJECT)

# Check depencies for rendering the manuscript
analysis/manuscript.pdf: analysis/manuscript.Rmd
analysis/manuscript.pdf: analysis/manuscript_files/apa.csl
analysis/manuscript.pdf: analysis/manuscript_files/r-references.bib
analysis/manuscript.pdf: analysis/manuscript_files/potato_masher.png

# Run the Docker container to render the manuscript
analysis/manuscript.pdf:
	$(run) Rscript -e "rmarkdown::render(input = '$(workdir)/analysis/manuscript.Rmd', knit_root_dir = '$(workdir)')"

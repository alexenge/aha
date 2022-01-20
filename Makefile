# Set some local variables
SHELL		:= bash
DOCKER_USER	:= alexenge
PROJECT		:= $(notdir $(CURDIR))
HOST_PATH 	:= $(CURDIR)
DCKR_PATH 	:= /home/rstudio/$(PROJECT)
DCKR_TAG 	:= $(DOCKER_USER)/$(PROJECT)

# If DOCKER=TRUE, do stuff inside in the Docker container
ifeq ($(DOCKER), TRUE)
	run := docker run --rm --volume $(HOST_PATH):$(DCKR_PATH) $(DCKR_TAG)
endif

# Knit the manuscript
all: analysis/manuscript.pdf
analysis/manuscript.pdf: analysis/manuscript.Rmd
analysis/manuscript.pdf: analysis/apa.csl
analysis/manuscript.pdf: analysis/references.bib
analysis/manuscript.pdf: analysis/r-references.bib
analysis/manuscript.pdf: analysis/example_stim.png
analysis/manuscript.pdf:
	$(run) Rscript -e "rmarkdown::render(input = 'analysis/manuscript.Rmd')"

# Build the docker DCKR
build: Dockerfile
	docker build --tag $(DCKR_TAG) .

# Save the docker image
save: $(PROJECT).tar.gz
$(PROJECT).tar.gz:
	docker save $(PROJECT):latest | gzip > $@

# Run an interactive RStudio session in the DCKR
interactive:
	docker run --rm --volume $(HOST_PATH):$(DCKR_PATH) \
	-e PASSWORD=1234 -p 8888:8888 $(DCKR_TAG)

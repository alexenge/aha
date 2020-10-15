# Set some local variables
PROJECT := $(notdir $(CURDIR))
HOST_PATH := $(CURDIR)/analysis
CONTAINER_PATH := /home/$(PROJECT)/analysis

# Build the Docker container
build: Dockerfile
	docker build --tag $(PROJECT) .
	
# Optionally save the Docker image
save: $(PROJECT).tar.gz
$(PROJECT).tar.gz:
	docker save $(PROJECT):latest | gzip > $@

# The default target is the manuscript
all: analysis/manuscript.pdf
    
# Check depencies for rendering the manuscript
analysis/manuscript.pdf: analysis/manuscript.Rmd
analysis/manuscript.pdf: analysis/manuscript_files/apa.csl
analysis/manuscript.pdf: analysis/manuscript_files/potato_masher.png
analysis/manuscript.pdf: analysis/manuscript_files/r-references.bib

# Run the Docker container to render the manuscript
analysis/manuscript.pdf:
	docker run --rm --volume $(HOST_PATH):$(CONTAINER_PATH) $(PROJECT) 


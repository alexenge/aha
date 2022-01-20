# User-defined variables
DOCKER_USER	:= alexenge

# Automatic workflow variables
SHELL		:= bash
PROJECT		:= $(notdir $(CURDIR))
HOST_PATH 	:= $(CURDIR)
DCKR_PATH 	:= /home/rstudio/proj
DCKR_TAG 	:= $(DOCKER_USER)/$(PROJECT)

# If DOCKER=TRUE, do stuff inside the Docker container
ifeq ($(DOCKER), TRUE)
	run := docker run --rm --volume $(HOST_PATH):$(DCKR_PATH) $(DCKR_TAG)
endif

# Knit the manuscript
all: analysis/manuscript.pdf
analysis/manuscript.pdf: analysis/manuscript.Rmd
analysis/manuscript.pdf:
	$(run) Rscript -e "rmarkdown::render(input = 'analysis/manuscript.Rmd')"

# Build the Docker container
build: Dockerfile
	docker build --tag $(DCKR_TAG) .

# Save the Docker image
save: $(PROJECT).tar.gz
$(PROJECT).tar.gz:
	docker save $(PROJECT):latest | gzip > $@

# Run an interactive RStudio session inside the Docker container
interactive:
	docker run --rm --volume $(HOST_PATH):$(DCKR_PATH) \
	-e PASSWORD=1234 -p 8888:8888 $(DCKR_TAG)

FROM rocker/binder:4.0.0

# Set some environment variables
ENV R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE \
    RSESSION_PROXY_RSTUDIO_1_4=yes



# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

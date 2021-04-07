FROM rocker/binder:4.0.0

# Set some environment variables
ENV R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE \
    RSTUDIO_VERSION=1.4.1103 \
    RSESSION_PROXY_RSTUDIO_1_4=yes

USER root
RUN /rocker_scripts/install_rstudio.sh
USER rstudio



# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

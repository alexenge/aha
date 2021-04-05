FROM asachet/rocker-stan:latest

# Set some environment variables
ARG NB_USER=rstudio
ARG NB_UID=1000
ENV USER=${NB_USER} \
    NB_UID=${NB_UID} \
    R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE

# Install some R and Python packages
ARG PAPAJA_VERSION=v0.1.0.9997
ARG EEGUTILS_VERSION=v0.5.0
RUN install2.r -s --error cowplot reticulate \
    && R -e "remotes::install_github('crsh/papaja', ref = '${PAPAJA_VERSION}')" \
    && R -e "remotes::install_github('craddm/eegUtils', ref = '${EEGUTILS_VERSION}')" \
    && apt-get update \
    && apt-get install -y python3-pip \
    && pip3 install mne==0.21.2 pandas==1.1.3 scikit-learn==0.23.2

# Set working directory
WORKDIR /home/${NB_USER}

# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

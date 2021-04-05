FROM asachet/rocker-stan:latest

# Install some R and Python packages
ARG PAPAJA_VERSION=v0.1.0.9997
ARG EEGUTILS_VERSION=v0.5.0
ENV R_REMOTES_UPGRADE=never
RUN install2.r -s --error cowplot reticulate \
  && R -e "remotes::install_github('crsh/papaja', ref = '${PAPAJA_VERSION}')" \
  && R -e "remotes::install_github('craddm/eegUtils', ref = '${EEGUTILS_VERSION}')" \
  && apt-get update \
  && apt-get install -y python3-pip \
  && pip3 install mne==0.21.2

# Set working directory
WORKDIR /home/rstudio

# Copy data and materials
COPY manuscript.Rmd .
COPY manuscript_files/ manuscript_files/
COPY materials/ materials/

# Give user permissions
RUN chown -R rstudio .

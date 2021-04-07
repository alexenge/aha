FROM rocker/binder:4.0.2

# Set some environment variables
ENV R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE \
    RSTUDIO_VERSION=1.2.5042

# Install RStudio, Stan and packages
COPY install_stan.R .
USER root
RUN /rocker_scripts/install_rstudio.sh &&\
    R install_stan.R && \
    R --quiet -e "remotes::install_github('crsh/papaja', ref = 'v0.1.0.9997')" && \
    R --quiet -e "remotes::install_github('craddm/eegUtils', ref = 'v0.5.0')" && \
    install2.r -s --error \
        brms \
        cowplot \
        reticulate \
        styler && \
    pip3 install --no-cache-dir \
        mne==0.21.2 \
        pandas==1.1.3 \
        scikit-learn==0.23.2

# Change user back to non-root
USER rstudio

# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

FROM rocker/binder:4.0.3

# Set some environment variables
ENV NB_USER=rstudio \
    R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE \
    RSTUDIO_VERSION=1.2.5042

# Install RStudio, clang, R and Python packages
USER root
COPY install_stan.R .
RUN /rocker_scripts/install_rstudio.sh && \
    apt-get install -y --no-install-recommends \
        clang && \
    mkdir .R && \
    echo "CXX14=clang++\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC" >> .R/Makevars && \
    install2.r -s --error \
        brms \
        cowplot \
        reticulate \
        rstan \
        styler && \
    R --quiet -e 'remotes::install_github("crsh/papaja", ref = "v0.1.0.9997")' && \
    R --quiet -e 'remotes::install_github("craddm/eegUtils", ref = "v0.5.0")' && \
    pip3 install --no-cache-dir \
        mne==0.21.2 \
        pandas==1.1.3 \
        scikit-learn==0.23.2

# Set working directory for knitr
RUN echo 'knitr::opts_knit$set(root.dir = getwd())' >> .Rprofile

# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

# Give user permissions
RUN chown -R ${NB_USER} .
USER ${NB_USER}

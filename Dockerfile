FROM rocker/binder:4.0.2

# Set environment variables
ENV NB_USER=rstudio \
    RETICULATE_MINICONDA_ENABLED=FALSE \
    RSTUDIO_VERSION=1.2.5042

# Install RStudio, clang, R and Python packages
USER root
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
    installGithub.r \
        crsh/papaja@0b4a9a79 \
        craddm/eegUtils@01c939f2 && \
    pip3 install --no-cache-dir \
        mne==0.21.2 \
        pandas==1.1.3 \
        scikit-learn==0.23.2

# Specify some startup instructions for R
RUN echo 'options(mc.cores = parallel::detectCores() - 1)' >> .Rprofile && \
    echo 'rstan::rstan_options(auto_write = TRUE)' >> .Rprofile && \
    echo 'knitr::opts_knit$set(root.dir = getwd())' >> .Rprofile

# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

# Give user permissions
RUN chown -R ${NB_USER} .
USER ${NB_USER}

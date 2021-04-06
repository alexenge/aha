FROM asachet/rocker-stan:latest

# Set some environment variables
ENV NB_USER=rstudio \
    NB_UID=1000 \
    HOME=/home/${NB_USER} \
    LD_LIBRARY_PATH=/usr/local/lib/R/lib \
    PAPAJA_VERSION=v0.1.0.9997 \
    EEGUTILS_VERSION=v0.5.0 \
    R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE

# Set working directory
WORKDIR ${HOME}

# Make some changes and install stuff as root
RUN echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron && \
    echo "export PATH=${PATH}" >> ${HOME}/.profile && \
    apt-get update && \
    apt-get install -y python3-pip

# Install some Python and R packages
RUN pip3 install --no-cache-dir \
        jupyter-rsession-proxy==1.2 \
        mne==0.21.2 \
        notebook==6.3.0 \
        pandas==1.1.3 \
        scikit-learn==0.23.2 && \
    R --quiet -e "remotes::install_github('IRkernel/IRkernel')" && \
    #R --quiet -e "IRkernel::installspec(prefix='${VENV_DIR}')" && \
    install2.r -s --error \
        cowplot \
        reticulate \
        styler && \
    R --quiet -e "remotes::install_github('crsh/papaja', ref = '${PAPAJA_VERSION}')" && \
    R --quiet -e "remotes::install_github('craddm/eegUtils', ref = '${EEGUTILS_VERSION}')"

# Expose user for binder
ARG NB_USER
ARG NB_UID
USER ${NB_USER}

# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

# Start notebook server
CMD jupyter notebook --ip 0.0.0.0

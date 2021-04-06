FROM asachet/rocker-stan:latest

# Set some environment variables
ENV NB_USER=rstudio \
    NB_UID=1000 \
    VENV_DIR=/srv/venv \
    PATH=${VENV_DIR}/bin:$PATH \
    LD_LIBRARY_PATH=/usr/local/lib/R/lib \
    PAPAJA_VERSION=v0.1.0.9997 \
    EEGUTILS_VERSION=v0.5.0 \
    R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE

# Make sure that nbrsessionproxy on binder can find the R environment
RUN echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron &&
    echo "export PATH=${PATH}" >> ${HOME}/.profile

# Set working directory
ENV HOME=/home/${NB_USER}
WORKDIR ${HOME}

# Expose user for binder
ARG NB_USER
ARG NB_UID
USER ${NB_USER}

# Install some R and Python packages
RUN install2.r -s --error \
        cowplot \
        reticulate \
        styler &&
    R --quiet -e "devtools::install_github('IRkernel/IRkernel')" &&
    R --quiet -e "IRkernel::installspec(prefix='${VENV_DIR}')" &&
    R -e "remotes::install_github('crsh/papaja', ref = '${PAPAJA_VERSION}')" &&
    R -e "remotes::install_github('craddm/eegUtils', ref = '${EEGUTILS_VERSION}')" &&
    mkdir -p ${VENV_DIR} &&
    chown -R ${NB_USER} ${VENV_DIR} &&
    apt-get update &&
    apt-get install -y python3-venv python3-dev &&
    apt-get purge &&
    apt-get clean &&
    rm -rf /var/lib/apt/lists/* &&
    python3 -m venv ${VENV_DIR} &&
    pip3 install --no-cache-dir \
        mne==0.21.2 \
        jupyter-rsession-proxy==1.2 \
        notebook==6.3.0 \
        pandas==1.1.3 \
        scikit-learn==0.23.2

# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

# Start notebook server
CMD jupyter notebook --ip 0.0.0.0

FROM rocker/binder:4.0.2

# Set some environment variables
ENV PAPAJA_VERSION=v0.1.0.9997 \
    EEGUTILS_VERSION=v0.5.0 \
    R_REMOTES_UPGRADE=never \
    RETICULATE_MINICONDA_ENABLED=FALSE

# # Install some Python and R packages
# RUN pip3 install --no-cache-dir \
#         mne==0.21.2 \
#         pandas==1.1.3 \
#         scikit-learn==0.23.2 && \
#     install2.r -s --error \
#         cowplot \
#         reticulate \
#         styler && \
#     R --quiet -e "remotes::install_github('crsh/papaja', ref = '${PAPAJA_VERSION}')" && \
#     R --quiet -e "remotes::install_github('craddm/eegUtils', ref = '${EEGUTILS_VERSION}')"

# Copy scripts, data, and materials
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/

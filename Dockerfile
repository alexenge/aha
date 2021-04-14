FROM alexenge/rstan:4.0.3

# Copy files into the container
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/
COPY requirements.txt .

# Install as root
USER root
RUN \
    # R packages from GitHub
    installGithub.r \
        craddm/eegUtils@01c939f2 \
        && \
    # Python packages
    pip3 install --no-cache-dir -r requirements.txt && \
    chown -R ${NB_USER} .

# Switch back to default user
USER ${NB_USER}

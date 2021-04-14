FROM alexenge/base_rstan:4.0.3

# Copy files into the container
COPY analysis/ analysis/
COPY data/ data/
COPY materials/ materials/
COPY requirements.txt .

# Switch to root user
USER root

# Install R Packages from Github
RUN installGithub.r \
        craddm/eegUtils@01c939f2 \
    # Install Python packages
    && pip3 install --no-cache-dir -r requirements.txt

# Switch back to default user
USER ${NB_USER}

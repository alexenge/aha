FROM alexenge/base_rstan:4.0.3

# Create new project directory
ENV PROJDIR="${HOME}/aha"
RUN mkdir "${PROJDIR}"
WORKDIR "${PROJDIR}"

# Copy files into the container
COPY code/ code/
COPY data/ data/
COPY materials/ materials/
COPY misc/ misc/
COPY requirements.txt .

# Switch to root user
USER root

# Install R Packages from Github
RUN installGithub.r \
        craddm/eegUtils@01c939f2 \
    # Install Python packages
    && pip3 install --no-cache-dir -r requirements.txt \
    # Install groupmne
    && pip3 install --no-cache-dir mutar==0.0.1 \
    && cd "${HOME}" \
    && git clone https://github.com/hichamjanati/groupmne \
    && cd groupmne \
    && python setup.py develop \
    && cd "${PROJDIR}"

# Switch back to default user
RUN chown -R "${NB_USER}" "${HOME}"
USER "${NB_USER}"

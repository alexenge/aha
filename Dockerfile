FROM alexenge/r_eeg:4.1.2

# COPY analysis/ analysis/
# COPY data/ data/
# COPY materials/ materials/
# COPY output/ output/

USER root

RUN \
    # Install latest version of the EEG pipeline
    pip install --upgrade --pre --index \
    https://test.pypi.org/simple/ hu-neuro-pipeline \
    # Add default user permissions
    && chown -R "$NB_USER" "$HOME"

USER "$NB_USER"

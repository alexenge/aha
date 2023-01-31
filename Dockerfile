FROM alexenge/r_eeg:4.2.1

COPY manuscript_files/ manuscript_files/
COPY materials/ materials/
COPY LICENSE.md .
COPY Makefile .
COPY manuscript.Rmd .
COPY README.md .

USER root

RUN \
    # Install latest version of the EEG pipeline
    pip install --upgrade git+https://github.com/alexenge/hu-neuro-pipeline.git@a318bf0 \
    # Add default user permissions
    && chown -R "$NB_USER" "$HOME"

USER "$NB_USER"

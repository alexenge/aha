FROM asachet/rocker-stan:3.6.1

# Install Python packages
RUN apt-get update \
  && apt-get install -y python3-pip \
  && pip3 install mne==0.20.8

# Set working directory inside the container
WORKDIR /home/rstudio

# Copy data and materials
COPY analysis/ analysis/
# COPY data/ data/
COPY materials/ materials/

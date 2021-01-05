FROM rocker/verse:4.0.2

WORKDIR /home/rstudio

COPY data/ data/
COPY materials/ materials/

ENV RENV_VERSION 0.12.0
RUN R -e "remotes::install_version('renv', version = '${RENV_VERSION}', repos = 'http://cran.us.r-project.org')"
COPY renv.lock renv.lock
RUN R -e "renv::consent(provided = TRUE)"
RUN R -e "renv::restore(prompt = FALSE)"

RUN apt-get update && apt-get install -y python3-pip
COPY requirements.txt requirements.txt
RUN pip3 install -r requirements.txt

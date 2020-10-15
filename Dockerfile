FROM rocker/verse:4.0.2

ENV RENV_VERSION 0.12.0
RUN R -e "remotes::install_version('renv', version = '${RENV_VERSION}', repos = 'http://cran.us.r-project.org')"

RUN apt-get update && apt-get install -y python3-pip && apt-get install -y python3-venv

WORKDIR /home/aha

COPY data/ data/

COPY renv.lock renv.lock
COPY requirements.txt requirements.txt
RUN R -e "renv::activate()"
RUN R -e "renv::restore(prompt = FALSE)"

ENTRYPOINT R -e "rmarkdown::render(input = 'analysis/manuscript.Rmd', knit_root_dir = getwd())"


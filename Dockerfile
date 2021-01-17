FROM rocker/verse:4.0.2

# Set working directory in the container
WORKDIR /home/rstudio

# Copy data and materials
COPY data/ data/
COPY materials/ materials/

# Restore R packages
COPY renv.lock renv.lock
ENV RENV_VERSION 0.12.0
RUN R -e "remotes::install_version('renv', version = '${RENV_VERSION}', repos = 'http://cran.us.r-project.org')"
RUN R -e "renv::consent(provided = TRUE)"
RUN R -e "renv::restore(prompt = FALSE)"

# Restore Python packages
COPY requirements.txt requirements.txt
RUN apt-get update && apt-get install -y python3-pip
RUN pip3 install -r requirements.txt

# Install LaTeX packages
RUN tlmgr update --self
RUN tlmgr install apa7 etoolbox iftex scalerel fp pdftexcmds pgf xcolor auxhook bigintcalc bitset etexcmds \
  gettitlestring hycolor hyperref intcalc kvdefinekeys kvsetkeys letltxmacro ltxcmds pdfescape refcount \
  rerunfilecheck stringenc uniquecounter zapfding infwarerr kvoptions booktabs threeparttable caption \
  fancyhdr endfloat float amsmath latex-amsmath-dev euenc fontspec tipa unicode-math xunicode grffile was \
  multirow threeparttablex trimspaces csquotes nowidow footmisc setspace filehook makecmds polyglossia \
  geometry environ

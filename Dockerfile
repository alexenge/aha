FROM alexenge/r_stan:4.0.5

ENV PROJDIR=${HOME}/aha
RUN mkdir ${PROJDIR}
WORKDIR ${PROJDIR}

COPY data/ data/
COPY manuscript/ manuscript/
COPY materials/ materials/
COPY requirements.txt .

USER root

RUN installGithub.r craddm/eegUtils@01c939f2 \
    && pip3 install --no-cache-dir -r requirements.txt \
    && pip3 install --no-cache-dir mutar==0.0.1 \
    && cd ${HOME} \
    && git clone https://github.com/hichamjanati/groupmne \
    && cd groupmne \
    && python setup.py develop \
    && cd ${PROJDIR} \
    && chown -R ${NB_USER} ${HOME}

USER ${NB_USER}

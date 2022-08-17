
FROM rocker/shiny-verse:4.1.0

LABEL base.image="rocker/shiny-verse"
LABEL dockerfile.version="2"
LABEL software="R Shiny Server"
LABEL software.version="4.1.0"
LABEL description="R Shiny Server for hosting Data Dashboards"
LABEL website=""
LABEL license=""
LABEL maintainer="Kelsey Florek"
LABEL maintainer.email="kelsey.florek@slh.wisc.edu"

# prevents having to enter commands during apt-get install
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  build-essential \
  libpoppler-cpp-dev \
  pkg-config \
  python-dev \
  libjpeg-dev \
  libpng-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libgdal-dev \
  libudunits2-dev

# install R packages
RUN R -e "install.packages(c(\
'shiny', \
'shinycssloaders', \
'plotly', \
'leaflet', \
'leaflet.minicharts', \
'dplyr', \
'rgdal', \
'RAthena', \
'lubridate', \
), repos = 'http://cran.us.r-project.org')"

RUN wget "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -O "awscliv2.zip" && unzip awscliv2.zip && sudo ./aws/install && rm awscliv2.zip

ENV CONDA_DIR /home/shiny/conda
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && /bin/bash ~/miniconda.sh -b -p $CONDA_DIR
ENV PATH=$CONDA_DIR/bin:$PATH
RUN conda init && pip install boto3

RUN rm -r /srv/shiny-server/*

# copy app into container
COPY .Rprofile /home/shiny/.Rprofile
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY seqTime /srv/shiny-server/seqTime
COPY seqTotal /srv/shiny-server/seqTotal
COPY geojsons /srv/shiny-server/assets/geojsons

EXPOSE 3838



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

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN apt-get update && apt-get upgrade -y && apt-get install -y \
  build-essential \
  libpoppler-cpp-dev \
  pkg-config \
  python-dev \
  libjpeg-dev

# install R packages
RUN R -e "install.packages(c(\
  'plotly',\
  'rjson',\
  'usmap',\
  'stringr',\
  'dplyr',\
  'shinydashboard',\
  'shinyWidgets',\
  'shinyBS',\
  'viridis',\
  'pdftools',\
  'paws',\
  'highcharter',\
  'shinycssloaders'), repos = 'http://cran.us.r-project.org')"

RUN mkdir /app && mkdir /data

# copy app into container
COPY *.R /app/
COPY www/ /app/www/
COPY geojsons/ /app/geojsons/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app',port=3838,host='0.0.0.0')"]

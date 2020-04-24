# Building on top of: rocker/geospatial:devel (r-devel)
FROM rocker/geospatial:devel

# Maintainer info
MAINTAINER Valentin Lucet <valentin.lucet@gmail.com>

# Copy the repo content
COPY . /rsyncrosim

# Install the dependencies
RUN apt-get -y update -qq \ 
  && apt-get install -y --no-install-recommends \
    libgsl0-dev \
  && R -e "install.packages(c('devtools', 'testthat', 'roxygen2'))" \
  && R -e "devtools::install_dev_deps('/rsyncrosim', dep = TRUE)"

# Building on top of: rocker/geospatial:devel (r-devel)
FROM rocker/geospatial:devel

# Maintainer info
MAINTAINER Valentin Lucet <valentin.lucet@gmail.com>

# Copy the repo content
COPY /rsyncrosim /home/rstudio/rsyncrosim
COPY /syncrosim_2_10 home/rstudio/syncrosim

# Edit Permissions
RUN chown -R rstudio /home/rstudio/rsyncrosim \
  && chown -R rstudio /home/rstudio/syncrosim

# Install the dependencies
RUN apt-get -y update -qq \
  && apt-get install -y --no-install-recommends \
    libgsl0-dev \
    mono-devel \
  && R -e "install.packages(c('devtools', 'testthat', 'roxygen2'))" \
  && R -e "devtools::install_dev_deps('/home/rstudio/rsyncrosim', dep = TRUE)"

# Set ENV var
ENV NOt_CRAN='true'

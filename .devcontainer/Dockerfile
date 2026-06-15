FROM rocker/rstudio:4.4.2

# System libraries required by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Set mc.cores to physical core count for all R sessions — enables mclapply
# parallelism automatically without per-session configuration
RUN echo 'options(mc.cores = parallel::detectCores(logical = FALSE))' \
    >> /usr/local/lib/R/etc/Rprofile.site

# Install all project R packages at build time so container startup is fast
RUN Rscript -e "install.packages(c( \
    'simmer', 'simmer.bricks', 'simmer.plot', \
    'sensitivity', 'triangle', 'truncnorm', \
    'dplyr', 'tidyr', 'ggplot2', 'patchwork', \
    'RColorBrewer', 'scales', 'stringr', 'reshape2', \
    'data.table', 'purrr', 'jsonlite', \
    'knitr', 'optparse', \
    'shiny', 'shinyBS' \
  ), repos = 'https://cloud.r-project.org', \
     Ncpus = parallel::detectCores())"

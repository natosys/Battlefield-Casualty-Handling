FROM rocker/rstudio:4.4.2

# System libraries required by R packages (curl, SSL, XML, graphics)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install all project R packages
RUN Rscript -e "install.packages(c( \
    'simmer', \
    'simmer.bricks', \
    'simmer.plot', \
    'sensitivity', \
    'triangle', \
    'truncnorm', \
    'jsonlite', \
    'dplyr', \
    'tidyr', \
    'stringr', \
    'ggplot2', \
    'patchwork', \
    'knitr', \
    'data.table', \
    'RColorBrewer', \
    'reshape2', \
    'scales', \
    'shiny', \
    'shinyBS', \
    'purrr', \
    'languageserver' \
), repos = 'https://cloud.r-project.org')"

# Set mc.cores to physical core count so mclapply uses full parallelism automatically
RUN echo "options(mc.cores = parallel::detectCores(logical = FALSE))" \
    >> /usr/local/lib/R/etc/Rprofile.site

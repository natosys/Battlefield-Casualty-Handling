FROM rocker/rstudio:4.4.2

# System libraries required by R packages (curl, SSL, XML, graphics, igraph/glpk,
# libuv runtime needed by the `fs` package's prebuilt binary)
RUN apt-get update && apt-get install -y \
    curl \
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
    libglpk-dev \
    libuv1t64 \
    && rm -rf /var/lib/apt/lists/*

# languageserver is a VS Code editor convenience tool, not a simulation dependency,
# so it is installed directly rather than tracked in renv.lock (Issue #72).
RUN Rscript -e "install.packages('languageserver', repos = 'https://cloud.r-project.org')"

# Install renv and pre-warm its global package cache from the committed lockfile.
# The cache lives under the rstudio user's home directory, outside the
# /home/rstudio/workspace bind mount (devcontainer.json), so it survives the
# runtime bind mount and lets `renv::restore()` in postCreateCommand resolve
# from cache instead of rebuilding packages from source on every container start.
RUN Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
COPY renv.lock /tmp/renv_bootstrap/renv.lock
RUN Rscript -e "renv::restore(project = '/tmp/renv_bootstrap', prompt = FALSE)"

# Set mc.cores to physical core count so mclapply uses full parallelism automatically
RUN echo "options(mc.cores = parallel::detectCores(logical = FALSE))" \
    >> /usr/local/lib/R/etc/Rprofile.site

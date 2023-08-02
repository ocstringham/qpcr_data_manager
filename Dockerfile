# Add the docker.io/rocker/shiny-verse image
FROM rocker/shiny-verse:latest

# spatial libs: https://github.com/rocker-org/rocker-versioned2/blob/master/scripts/install_geospatial.sh
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    gdal-bin \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev

# Install Git
RUN apt-get update && \
    apt-get install -y git

# Install additional R packages required
RUN R -e "install.packages(c('shinyWidgets', 'htmltools', 'zip', 'reactable', 'scales', 'sf', 'leaflet', 'leaflet.extras2'), Ncpus=4)"

# Clone the Shiny app from GitHub
RUN git clone https://github.com/ocstringham/qpcr_data_manager /srv/shiny-server/qpcr_data_manager

# Expose the default Shiny Server port (3838)
EXPOSE 3838

# Start the Shiny Server
CMD ["/usr/bin/shiny-server"]

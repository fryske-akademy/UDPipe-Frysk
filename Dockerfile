# Use the Rocker project’s Shiny base image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y libxml2-dev
RUN apt-get install -y libicu-dev
RUN apt-get install -y libudunits2-dev
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y libwebp-dev
RUN apt-get install -y libpoppler-cpp-dev
RUN apt-get install -y pkg-config
RUN apt-get install -y libtesseract-dev
RUN apt-get install -y libleptonica-dev
RUN apt-get install -y libprotobuf-dev
RUN apt-get install -y protobuf-compiler
RUN apt-get install -y glpk-utils
RUN apt-get install -y libglpk-dev
RUN apt-get install -y g++
RUN rm -rf /var/lib/apt/lists/*

# Copy your app to the container
COPY ./ /srv/shiny-server/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Install R package dependencies
RUN R -e "install.packages('shiny',        dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyjs',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyWidgets', dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyAce',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('readr',        dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('readtext',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('xml2',         dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rvest',        dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('openxlsx',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('stringr',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('cld3',         dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('udpipe',       dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('DT',           dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('lattice',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('igraph',       dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggraph',       dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggplot2',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('textrank',     dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggwordcloud',  dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('RJSONIO',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('remotes',      dependencies=TRUE, repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('AnalytixWare/ShinySky')"

# Expose the Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]

# our R base image
FROM r-base

# create an R user
ENV HOME /home/ghgvcr
RUN useradd --create-home --home-dir $HOME ghgvcr \
    && mkdir $HOME/data \
    && chown -R ghgvcr:ghgvcr $HOME

# install distro libraries for R dependencies
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		libnetcdf-dev libxml2 libxml2-dev libssl-dev \
	&& rm -rf /var/lib/apt/lists/*

# Set workdir & user early in the application, so that all libs are installed
# as ghgvcr user, rather than as root
WORKDIR $HOME
USER ghgvcr

# Create a lib folder under the ghgvcr $HOME, so the ghgvcr user has the ability
# to write to it. Useful later if we need to install libs for testing
RUN mkdir -p /home/ghgvcr/lib

# Set the R lib path for the user. All R libs will be installed here
ENV R_LIBS_USER /home/ghgvcr/lib

# install R dependency packages
RUN Rscript -e "install.packages(c('ggplot2', 'gridExtra', 'Hmisc', 'jsonlite', 'scales', 'tidyr', 'ncdf4', 'Rserve', 'XML', 'readr', 'rmarkdown', 'testthat'), repos = 'http://cran.us.r-project.org')"

# place the ghgvcR project into the image
COPY . $HOME

# install our project packages
RUN Rscript -e "install.packages('$HOME', repos=NULL, type='source')"

EXPOSE 6311

# set the command
CMD Rscript start.R

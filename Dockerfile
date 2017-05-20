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
		libnetcdf-dev \
	&& rm -rf /var/lib/apt/lists/*

# install R dependency packages
RUN echo "install.packages(c('ggplot2', 'gridExtra', 'Hmisc', 'jsonlite', 'scales', 'tidyr', 'ncdf4', 'Rserve'), repos = 'http://cran.us.r-project.org')" > /tmp/packages.R \
    && Rscript /tmp/packages.R

# place the ghgvcR project into the image
COPY . $HOME

# install our project packages
RUN echo "install.packages('$HOME', repos=NULL, type='source')" > /tmp/packages.R \
    && Rscript /tmp/packages.R

WORKDIR $HOME
USER ghgvcr

EXPOSE 6311

# set the command
CMD Rscript start.R

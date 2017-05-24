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
RUN Rscript -e "install.packages(c('ggplot2', 'gridExtra', 'Hmisc', 'jsonlite', 'scales', 'tidyr', 'ncdf4', 'Rserve', 'XML'), repos = 'http://cran.us.r-project.org')"

# place the ghgvcR project into the image
COPY . $HOME

# install our project packages
RUN Rscript -e "install.packages('$HOME', repos=NULL, type='source')"

WORKDIR $HOME
USER ghgvcr

EXPOSE 6311

# set the command
CMD Rscript start.R

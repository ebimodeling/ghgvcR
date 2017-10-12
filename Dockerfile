FROM jaydorsey/ghgvcr-base

# place the ghgvcR project into the image
COPY . $HOME

# install our project packages
RUN Rscript -e "install.packages('$HOME', repos=NULL, type='source')"

EXPOSE 6311

# set the command
CMD Rscript start.R

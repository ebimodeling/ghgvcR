FROM ebimodeling/ghgvcr-base

COPY --chown=ghgvcr-user:ghgvcr-user . $APP_PATH

RUN Rscript -e "install.packages('$APP_PATH', repos=NULL, type='source')"

EXPOSE 6311

# set the command
CMD Rscript start.R

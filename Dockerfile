# The ghgvcr-base image installs R and all the dependencies needed to run
# the scripts
FROM jaydorsey/ghgvcr-base

# Copy the ghgvc R code into the image
#
# TODO: Once the Docker for mac client is updated, use this syntax to reduce
# the number of layers
#
# COPY . $APP_PATH --chown=$USER:$USER
USER root
COPY . $APP_PATH
RUN chown -R $USER:$USER $APP_PATH

USER $USER

# Install our project package
RUN Rscript -e "install.packages('$APP_PATH', repos=NULL, type='source')"

ENTRYPOINT ["bin/docker-entrypoint.sh"]

EXPOSE 6311

CMD ["bin/start-server.sh"]

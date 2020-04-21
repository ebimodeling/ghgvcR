#!/usr/bin/env bash

echo "checking for app data"
if [ ! -f /app/data/netcdf/saatchi.nc ]; then
  echo "file doesn't exist"
  curl -L -o /app/data/app_netcdf_data.zip https://storage.googleapis.com/ghgvcdata/app_netcdf_data.zip
  unzip -o /app/data/app_netcdf_data.zip -d /app/data
  rm /app/data/app_netcdf_data.zip
  chown -R ghgvcr-user:ghgvcr-user /app/data/netcdf
fi

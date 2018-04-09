#!/usr/bin/env bash

echo "checking for app data"
if [ ! -f /app/data/name_indexed_ecosystems.json ]; then
  echo "file doesn't exist"
  curl -L -o /app/data/app_data.zip https://www.dropbox.com/s/2g2x58fkolgnn76/app_data.zip?dl=1
  unzip -o /app/data/app_data.zip -d /app/data/
  rm /app/data/app_data.zip
  chown -R ghgvcr-user:ghgvcr-user /app/data/
fi

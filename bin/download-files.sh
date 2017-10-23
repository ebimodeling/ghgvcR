#!/usr/bin/env sh

# This is a wrapper file that downloads the data files if not present
set -e

if [ ! -f $DATA_PATH/name_indexed_ecosystems.json ]; then
  curl -L -o $DATA_PATH/app_data.zip https://www.dropbox.com/s/2g2x58fkolgnn76/app_data.zip?dl=1
  unzip $DATA_PATH/app_data.zip -d $DATA_PATH
  rm $DATA_PATH/app_data.zip
else
  echo "Data file already downloaded & unzipped"
fi

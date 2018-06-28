#!/bin/bash
#Exit immediately if any command returns a non zero exit status:
set -e

#Read the output directory from line 2 of the pathnames file:
OUTPUT_DIRECTORY="$(sed -n '2p' pathnames)"

#Write the environment (flexpart config file) to the output directory:
touch "${OUTPUT_DIRECTORY}flexpart.conf"
cat > "${OUTPUT_DIRECTORY}flexpart.conf" << ENVIRONMENT
INPUT_DATA_MOUNT_POINT=${INPUT_DATA_MOUNT_POINT}
OUTPUT_DATA_MOUNT_POINT=${OUTPUT_DATA_MOUNT_POINT}
RUN_NAME=${RUN_NAME}
DIRECTION=${DIRECTION}
START_DATE=${START_DATE}
START_HOUR=${START_HOUR}
RUN_LENGTH=${RUN_LENGTH}
LATITUDE=${LATITUDE}
LONGITUDE=${LONGITUDE}
ENVIRONMENT

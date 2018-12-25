#!/bin/bash
#Exit immediately if any command returns a non zero exit status:
set -e

#Check if (user defined) variable RUN_NAME was set correctly:
if [[ ! ${RUN_NAME} =~ ^[A-Za-z0-9_-]{1,246}$ ]] ; then
  echo "ERROR: Incorrect format in RUN_NAME!" 1>&2
  echo "       RUN_NAME=\"${RUN_NAME}\"" 1>&2
  echo "       May contain alphanumeric characters plus underscores and dashes. Must not
               be empty, and no more than 246 characters in length.)" 1>&2
  exit 1
fi

#Check if the (user defined) INPUT_DATA_MOUNT_POINT directory exists:
if [[ ! -d /flexpart_input/ ]]; then
  echo "ERROR: INPUT_DATA_MOUNT_POINT directory not found!" 1>&2
  echo "       INPUT_DATA_MOUNT_POINT=\"${INPUT_DATA_MOUNT_POINT}\"" 1>&2
  echo "       Perhaps the input volume was not mounted correctly?" 1>&2
  exit 1
fi

#Check if the (user defined) OUTPUT_DATA_MOUNT_POINT directory exists:
if [[ ! -d ${OUTPUT_DATA_MOUNT_POINT} ]]; then
  echo "ERROR: OUTPUT_DATA_MOUNT_POINT directory not found!" 1>&2
  echo "       OUTPUT_DATA_MOUNT_POINT=\"${OUTPUT_DATA_MOUNT_POINT}\"" 1>&2
  echo "       Perhaps the output volume was not mounted correctly?" 1>&2
  exit 1
fi

#Create the run specific output directory:
OUTPUT_DIRECTORY="$(mktemp -d ${RUN_NAME}-XXXXXXXX -p /flexpart_output/)"

#Overwrite the pathnames FLEXPART config file:
cat > pathnames << PATHNAMES
/flexpart_model/options/
${OUTPUT_DIRECTORY}/
${INPUT_DATA_MOUNT_POINT}/
${INPUT_DATA_MOUNT_POINT}/AVAILABLE
============================================
PATHNAMES

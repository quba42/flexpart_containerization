#!/bin/bash
# Convenience script for running the FLEXPART Docker container.

IMAGE_VERSION='alpendac'
IMAGE_NAME='flexpart/flexpart'
CONTAINER_NAME='flexpart-run'
INPUT_VOLUME='flexpart_input_local'
OUTPUT_VOLUME='flexpart_output_local'
REPOSITORY_ROOT=$(git rev-parse --show-toplevel)
ENV_FILE="${REPOSITORY_ROOT}/env_files/flexpart.env"
HELP_TEXT="\
./run-flexpart-alpendac-prototype.sh <options>

  -h    display this help message;
  -i    run the docker container interactively;"

while getopts "ih" option "$@" ; do
  case $option in
    i) #run the container interactively:
      EXTRA_OPTS='-it --entrypoint=bash'
      ;;
    h)
      echo "$HELP_TEXT"
      exit 0
      ;;
    \?)
      echo "Invalid option -$OPTARG." >&2
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done

docker run ${EXTRA_OPTS}\
 --name ${CONTAINER_NAME}\
 --rm\
 --env-file ${ENV_FILE}\
 --mount source=${INPUT_VOLUME},target=/flexpart_input\
 --mount source=${OUTPUT_VOLUME},target=/flexpart_output\
 ${IMAGE_NAME}:${IMAGE_VERSION}

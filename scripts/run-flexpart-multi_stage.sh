#!/bin/bash
#convenience script for running the flexpart/multi_stage container.

#constants:
IMAGE_VERSION='main'
IMAGE_NAME='flexpart/alpendac-prototype'
CONTAINER_NAME='flexpart-run'
INPUT_VOLUME='flexpart_input_local'
OUTPUT_VOLUME='flexpart_output_local'
ENV_FILE='flexpart.conf'
HELP_TEXT="\
./run-flexpart-alpendac-prototype.sh <options>

  -h    display this help message;
  -i    run the docker container interactively;"

#options processing:
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

#commands:
docker run ${EXTRA_OPTS}\
 --name ${CONTAINER_NAME}\
 --rm\
 --env-file ${ENV_FILE}\
 --mount source=${INPUT_VOLUME},target=/flexpart_input\
 --mount source=${OUTPUT_VOLUME},target=/flexpart_output\
 ${IMAGE_NAME}:${IMAGE_VERSION}

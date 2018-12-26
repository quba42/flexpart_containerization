#!/bin/bash
# Convenience script for running the QuickLook Docker container.

IMAGE_VERSION='beta'
IMAGE_NAME='flexpart/quicklook'
CONTAINER_NAME='quicklook-run'
OUTPUT_VOLUME='flexpart_output_local'
EXTRA_OPTS=''
PASS_THROUGH_OPTS=''
HELP_TEXT="\
./run-flexpart-alpendac-prototype.sh <options>

  -h    display this help message;
  -i    run the docker container interactively;
  -n <CONTAINER_NAME>
        supply an alternate container name;
  -o <FLEXPART_OUTPUT_FOLDER>
        pass the flexpart output folder to the container;
  -e    pass the -e argument to the container;
  -v    pass the -v argument to the container;"

while getopts "ihn:o:ev" option "$@" ; do
  case $option in
    v)
      PASS_THROUGH_OPTS='-v'
      ;;
    e)
      PASS_THROUGH_OPTS='-e'
      ;;
    o)
      PASS_THROUGH_OPTS="${PASS_THROUGH_OPTS} -o ${OPTARG}"
      ;;
    n)
      CONTAINER_NAME="${OPTARG}"
      ;;
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
 --mount source=${OUTPUT_VOLUME},target=/flexpart_output\
 ${IMAGE_NAME}:${IMAGE_VERSION}\
 ${PASS_THROUGH_OPTS}

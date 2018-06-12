#!/bin/bash
#convenience script for running the flexpart/multi_stage container.

# constants:

IMAGE_VERSION='main'
IMAGE_NAME='flexpart/multi_stage'
CONTAINER_NAME='flexpart-prototype'
INPUT_VOLUME='flexpart_input_local'
OUTPUT_VOLUME='flexpart_output_local'

# commands:

docker run\
 --name ${CONTAINER_NAME}\
 -it --entrypoint=bash\
 --mount source=${INPUT_VOLUME},target=/flexpart_input\
 --mount source=${OUTPUT_VOLUME},target=/flexpart_output\
 ${IMAGE_NAME}:${IMAGE_VERSION}

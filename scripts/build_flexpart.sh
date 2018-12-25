#!/bin/bash
#Convenience script for building the flexpart/alpendac-prototype image with
#fully explicit tag.

#Constants:

VERSION='main'
BASE_NAME='flexpart'
IMAGE_NAME='alpendac-prototype'
FULL_NAME="${BASE_NAME}/${IMAGE_NAME}"
SOURCE_DIR="alpendac_prototype"

#Commands:

docker build "${SOURCE_DIR}" -t "${FULL_NAME}:${VERSION}"

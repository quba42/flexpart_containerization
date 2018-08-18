#!/bin/bash
#Convenience script for building the flexpart/quicklook image with
#fully explicit tag.

#Constants:

VERSION='main'
BASE_NAME='flexpart'
IMAGE_NAME='quicklook'
FULL_NAME="${BASE_NAME}/${IMAGE_NAME}"
SOURCE_DIR="quicklook"

#Commands:

docker build "${SOURCE_DIR}" -t "${FULL_NAME}:${VERSION}"

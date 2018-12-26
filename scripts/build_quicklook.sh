#!/bin/bash
# Convenience script for building the QuickLook Docker image

VERSION='beta'
BASE_NAME='flexpart'
IMAGE_NAME='quicklook'
FULL_NAME="${BASE_NAME}/${IMAGE_NAME}"
REPOSITORY_ROOT=$(git rev-parse --show-toplevel)
IMAGE_SOURCE_DIR="${REPOSITORY_ROOT}/image_sources/quicklook"

docker build "${IMAGE_SOURCE_DIR}" -t "${FULL_NAME}:${VERSION}"

#!/bin/bash
# Convenience script for building the FLEXPART Docker image

VERSION='alpendac'
BASE_NAME='flexpart'
IMAGE_NAME='flexpart'
FULL_NAME="${BASE_NAME}/${IMAGE_NAME}"
REPOSITORY_ROOT=$(git rev-parse --show-toplevel)
IMAGE_SOURCE_DIR="${REPOSITORY_ROOT}/image_sources/flexpart"

docker build "${IMAGE_SOURCE_DIR}" -t "${FULL_NAME}:${VERSION}"

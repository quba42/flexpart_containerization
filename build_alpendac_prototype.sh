#!/bin/bash
#Convenience script for building the flexpart/multi_stage container with proper
#versioning.

#Constants:

VERSION='main'
NAME='flexpart/alpendac-prototype'
SOURCE_DIR="../image_sources/${NAME}"

#Commands:

docker build "${SOURCE_DIR}" -t "${NAME}:${VERSION}"

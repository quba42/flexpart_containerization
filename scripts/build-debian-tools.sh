#!/bin/bash
#convenience script for building the flexpart/multi_stage container with proper
#versioning.

# constants:

VERSION='main'
NAME='debian/tools'
SOURCE_DIR="../image_sources/${NAME}"

# commands:

docker build "${SOURCE_DIR}" -t "${NAME}:${VERSION}"

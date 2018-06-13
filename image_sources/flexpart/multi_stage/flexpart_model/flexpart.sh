#!/bin/bash

ulimit -s unlimited
mkdir -p /flexpart_output/output

exec "$@"

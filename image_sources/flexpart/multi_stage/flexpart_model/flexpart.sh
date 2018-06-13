#!/bin/bash

ulimit -s unlimited
export OMP_STACKSIZE=512M

mkdir -p /flexpart_output/output

exec "$@"

#!/bin/bash

ulimit -s unlimited
export OMP_STACKSIZE=512M

mkdir -p /flexpart_output/output

./flexpart.gfs | tee /flexpart_output/flexpart.out

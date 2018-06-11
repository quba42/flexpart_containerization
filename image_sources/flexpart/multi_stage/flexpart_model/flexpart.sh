#!/bin/bash

ulimit -s unlimited
export OMP_STACKSIZE=512M

./flexpart.gfs | tee flexpart.out

#!/bin/bash

./01-mkpathnames.sh
./flexpart.gfs | tee flexpart.out

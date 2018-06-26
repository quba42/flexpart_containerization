#!/bin/bash
#Exit immediately if any command returns a non zero exit status:
set -e

#FLEXPART wants ulimit to be set:
ulimit -s unlimited

#Overwrite FLEXPART config files using ENV parameters:
configure_COMMAND.sh
configure_RELEASES.sh
configure_pathnames.sh

#Execute the FLEXPART run:
exec "$@"

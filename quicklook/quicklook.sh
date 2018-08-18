#!/bin/bash
#Exit immediately if any command returns a non zero exit status:
set -e

#Default parameters:
RUN_MODE="-i"
HELP_TEXT="\
This Docker container (containing quick_look.py) supports the following non-
docker options:

  -o <FLEXPART_OUTPUT_FOLDER>
        The name of the folder containing the flexpart output data;
        e.g.: \"flexpart_run-Y3VhPm8A\";
        The argument should be just the folder name (may not contain \"/\");
        May alternatively be supplied by the corresponding environmental variable;
  -e    Use exploration mode (quick_look.py will print information about the
        Flexpart run supplied and exit);
  -h    Print this help text and exit;
  -v    Print the version of quick_look.py and exit;

All other quick_look.py options must be supplied to the container via
environmental variable parameters. See the Dockerfile or the container
documentation (see also https://github.com/Quba42/flexpart_containerization)
for more information."

#Use Agg backend instead of Xwindows to remove the need for a display:
set_matplotlib_backend.sh

#Options processing:
while getopts "ehvo:" option "$@" ; do
  case $option in
    o)
      FLEXPART_OUTPUT_FOLDER="${OPTARG}"
      ;;
    e)
      RUN_MODE="-e"
      ;;
    h)
      echo "${HELP_TEXT}"
      exit 0
      ;;
    v)
      quick_look.py --version
      exit 0
      ;;
    \?)
      echo "Invalid option -${OPTARG}." >&2
      exit 1
      ;;
    :)
      echo "Option -${OPTARG} requires an argument." >&2
      exit 1
      ;;
  esac
done

#Perform sanity checks on user supplied parameters:
if [[ ! "${FLEXPART_OUTPUT_FOLDER}" ]] ; then
  echo "ERROR: Flexpart output folder is not set!" 1>&2
  echo "       FLEXPART_OUTPUT_FOLDER=\"\"" 1>&2
  echo "       Should be something like  \"flexpart_run-Y3VhPm8A\"" 1>&2
  exit 1
fi

if [[ ! -d "${FLEXPART_OUTPUT_MOUNT_POINT}/${FLEXPART_OUTPUT_FOLDER}" ]] ; then
  echo "ERROR: Flexpart output directory does not exist!" 1>&2
  echo "       FLEXPART_OUTPUT_MOUNT_POINT=\"${FLEXPART_OUTPUT_MOUNT_POINT}\"" 1>&2
  echo "       FLEXPART_OUTPUT_FOLDER=\"${FLEXPART_OUTPUT_FOLDER}\"" 1>&2
  echo "       RESULTANT_PATH=\"${FLEXPART_OUTPUT_MOUNT_POINT}/${FLEXPART_OUTPUT_FOLDER}\"" 1>&2
  exit 1
fi

if [[ ${LOWER_LEFT_LONGITUDE} && ${LOWER_LEFT_LATITUDE} && ${UPPER_RIGHT_LONGITUDE} && ${UPPER_RIGHT_LATITUDE} ]] ; then
  DOMAIN="--domain="
  CANVAS_COORDINATES="${LOWER_LEFT_LONGITUDE} ${LOWER_LEFT_LATITUDE} ${UPPER_RIGHT_LONGITUDE} ${UPPER_RIGHT_LATITUDE}"
else
  DOMAIN=""
  MAXIMUM_GRID="TRUE"
fi

if [[ "${MAXIMUM_GRID}" == "TRUE" ]] ; then
  MAXIMUM_GRID_OPTION="-m"
else
  MAXIMUM_GRID_OPTION=""
fi

#Run quick_look.py:
eval "quick_look.py \
  ${RUN_MODE} ${FLEXPART_OUTPUT_MOUNT_POINT}/${FLEXPART_OUTPUT_FOLDER} \
  ${DOMAIN}${CANVAS_COORDINATES} \
  ${MAXIMUM_GRID_OPTION} \
  -l ${LAYERS} \
  -t ${DOMAIN_TYPE} \
  -z ${PROJECTION} \
  -s ${SPECIES}"

#!/bin/bash
#Exit immediately if any command returns a non zero exit status:
set -e

#Release length in hours (must be less than 24 for this script to work):
RELEASE_LENGTH='2'

#Check the START_DATE format:
if [[ ! ${START_DATE} =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] && date -d "${START_DATE}" ; then
  echo "ERROR: Incorrect format in START_DATE!" 1>&2
  echo "       START_DATE=\"${START_DATE}\"" 1>&2
  echo "       Must be of the form YYYY-MM-DD." 1>&2
  exit 1
fi

#Check the START_HOUR format:
if [[ ! ${START_HOUR} =~ ^(0[0-9]|1[0-9]|2[0-3])$ ]] ; then
  echo "ERROR: Incorrect format in START_HOUR!" 1>&2
  echo "       START_HOUR=\"${START_HOUR}\"" 1>&2
  echo "       Must be of the form HH (between 00 and 23)" 1>&2
  exit 1
fi

#Process variables dependent on the run DIRECTION:
if [[ ${DIRECTION} == 1 ]] ; then
  HOURS_REMAINING=$(( 24 - 10#${START_HOUR} ))
  if [[ 10#${RELEASE_LENGTH} -ge 10#${HOURS_REMAINING} ]]; then
    END_DATE="$( date -d "${START_DATE}+1 days" --iso-8601=date )"
    END_HOUR="$(( 10#${RELEASE_LENGTH} - 10#${HOURS_REMAINING} ))"
  else
    END_DATE="${START_DATE}"
    END_HOUR="$(( 10#${START_HOUR} + 10#${RELEASE_LENGTH} ))"
  fi
  END_HOUR="$( printf "%02d" "${END_HOUR}" )"
elif [[ ${DIRECTION} == -1 ]] ; then
  END_DATE="${START_DATE}"
  END_HOUR="${START_HOUR}"
  if [[ 10#${RELEASE_LENGTH} -gt 10#${END_HOUR} ]]; then
    START_DATE="$( date -d "${END_DATE}-1 days" --iso-8601=date )"
    OVERFLOW_HOURS="$(( 10#${RELEASE_LENGTH} - 10#${END_HOUR} ))"
    START_HOUR="$(( 24 - 10#${OVERFLOW_HOURS} ))"
  else
    #START_DATE is already the same as END_DATE
    START_HOUR="$(( 10#${END_HOUR} - 10#${RELEASE_LENGTH} ))"
  fi
  START_HOUR="$( printf "%02d" "${START_HOUR}" )"
else
  echo "ERROR: Incorrect format in DIRECTION!" 1>&2
  echo "       DIRECTION=\"${DIRECTION}\"" 1>&2
  echo "       Must be either 1 or -1!" 1>&2
  exit 1
fi

#Process variables not dependent on the run DIRECTION:
START_TIME="${START_HOUR}0000"
END_TIME="${END_HOUR}0000"

#Strip the dashes back out of the dates for FLEXPART compatibility:
START_DATE=$(echo "${START_DATE}" | sed 's/-//g')
END_DATE=$(echo "${END_DATE}" | sed 's/-//g')

#Check the LATITUDE format:
if [[ ! ${LATITUDE} =~ ^-?([0-9](.[0-9]{1,2})?|[1-8][0-9](.[0-9]{1,2})?|90(.00)?)$ ]] ; then
  echo "ERROR: Incorrect format in LATITUDE!" 1>&2
  echo "       LATITUDE=\"${LATITUDE}\"" 1>&2
  echo "       Must be between -90 and 90, and may have up to two decimal places." 1>&2
  exit 1
fi

#Check LATITUDE for excessive proximity to the poles:
if [[ ${LATITUDE} =~ ^-?(89.7[5-9]|89.[89]?|90(.00)?)$ ]] ; then
  echo "ERROR: Incorrect format in LATITUDE!" 1>&2
  echo "       LATITUDE=\"${LATITUDE}\"" 1>&2
  echo "       Currently this model does not support releases within .25 degrees of the" 1>&2
  echo "       poles." 1>&2
  exit 1
fi

#Check the LONGITUDE format:
if [[ ! ${LONGITUDE} =~ ^-?([0-9](.[0-9]{1,2})?|[1-9][0-9](.[0-9]{1,2})?|1[0-7][0-9](.[0-9]{1,2})?|180)$ ]] ; then
  echo "ERROR: Incorrect format in LONGITUDE!" 1>&2
  echo "       LONGITUDE=\"${LONGITUDE}\"" 1>&2
  echo "       Must be between -180 and 180, and may have up to two decimal places." 1>&2
  exit 1
fi

#Releases are spread up to .25 degrees around the user supplied coordinates:
UPPER_LATITUDE=$(echo "${LATITUDE} + 0.25" | bc)
LOWER_LATITUDE=$(echo "${LATITUDE} - 0.25" | bc)
UPPER_LONGITUDE=$(echo "${LONGITUDE} + 0.25" | bc)
LOWER_LONGITUDE=$(echo "${LONGITUDE} + -.25" | bc)

#Overwrite the options/RELEASES FLEXPART config file:
cat > options/RELEASES << RELEASES
*************************************************************************
*                                                                       *
*   Input file for the Lagrangian particle dispersion model FLEXPART    *
*                        Please select your options                     *
*                                                                       *
*   Kind of trajectory: 1 = 3-dimensional                               *
*                       2 = 2-dimensional (terrain-following)           *
*                                                                       *
*                                                                       *
*************************************************************************
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
01     Total Number of Species being emitted
24     Species index (according to file SPECIES)
=========================================================================
${START_DATE} ${START_TIME}
${END_DATE} ${END_TIME}
${LOWER_LONGITUDE}
${LOWER_LATITUDE}
${UPPER_LONGITUDE}
${UPPER_LATITUDE}
2               z (height) Code: 1-> m above ground, 2-> m above sea level, 3-> pressure in hPa
1590            z_lower
2090            z_upper
10000           Total number of particles to be released
0.0001E00       Total mass emitted
UFSsite
RELEASES

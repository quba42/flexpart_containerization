#!/bin/bash
#Exit immediately if any command returns a non zero exit status:
set -e

#Check the RUN_LENGTH format:
if [[ ! ${RUN_LENGTH} =~ ^[1-9]$ ]] ; then
  echo "ERROR: Incorrect format in RUN_LENGTH!" 1>&2
  echo "       RUN_LENGTH=\"${RUN_LENGTH}\"" 1>&2
  echo "       Currently the run length is limited to between 1 and 9 days." 1>&2
  exit 1
fi

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
  END_DATE=$(date -d "${START_DATE}+${RUN_LENGTH} days" --iso-8601=date)
elif [[ ${DIRECTION} == -1 ]] ; then
  END_DATE=${START_DATE}
  START_DATE=$(date -d "${END_DATE}-${RUN_LENGTH} days" --iso-8601=date)
else
  echo "ERROR: Incorrect format in DIRECTION!" 1>&2
  echo "       DIRECTION=\"${DIRECTION}\"" 1>&2
  echo "       Must be either 1 or -1!" 1>&2
  exit 1
fi

#Process variables not dependent on the run DIRECTION:
END_HOUR="${START_HOUR}"
START_TIME="${START_HOUR}0000"
END_TIME="${END_HOUR}0000"

#Strip the dashes back out of the dates for FLEXPART compatibility:
START_DATE=$(echo "${START_DATE}" | sed 's/-//g')
END_DATE=$(echo "${END_DATE}" | sed 's/-//g')

#Overwrite the options/COMMAND FLEXPART config file:
cat > options/COMMAND << COMMAND







${DIRECTION}                                   DIRECTION
${START_DATE} ${START_TIME}
${END_DATE} ${END_TIME}
07200                                SSSSS         OUTPUT EVERY SSSSS SECONDS
07200                                SSSSS          TIME AVERAGE OF OUTPUT
00900                                SSSSS          SAMPLING RATE DF OUTPUT
999999                               SSSSSSS       TIME CONSTANT FOR PARTICLE SPLITTING (IN SECONDS)
00900                                SSSSS           SYNCHRONISATION INTERVAL OF FLEXPART (IN SECONDS)
-5 CTL                               FACTOR, BY WHICH TIME STEP MUST BE SMALLER THAN TL
4                                    IFIHE         DECREASE OF TIME STEP FOR VERTICAL MOTION BY FACTOR IFINE
5                                    IOUT          1 CONC, 2 MIXING RATIO, 3 BOTH 4 PLUME TRAJECT.,5 = 1 and 4 same time
0                                    IPOUT           PARTICLE DUMP: 0 NO, 1 EVERY OUTPUT INTERVAL, 2 ONLY AT END
1                                    LSUBGRID       SUBGFUD TERRAIN EFFECT PARAMETERIZATION: 1 YES, 8 NO
1                                    LCONVECTION     CONVECTION: 1 YES, 0 NO
0                                    LAGESPECTRA    AGE SPECTRA: 1 YES, 0 NO
0                                    IRIN           CONTINUE SIMULATION WITH DUMPED PARTICLE DATA: 1 YES, 0 NO
1                                    OUTPUTFOREACHRELEASE.: 1 YES, 0 NO
0                                    IFLUX          CALCULATE FLUXES: 1 YES, 0 NO
0                                    MDOMAINFILL
1                                    IND_SOURCE
1                                    IND_RECEPTOR
0                                    MQUASILAG
0                                    NESTED_OUTPUT
1                                    LINIT_COND
COMMAND

#This container is used to compile FLEXPART:
FROM debian:stretch AS builder
ADD flexpart_code /flexpart_code
WORKDIR /flexpart_code
RUN apt-get update && apt-get install -y \
    gfortran \
    libgrib-api-dev \
    build-essential \
 && echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list \
 && apt-get update && apt-get install -y \
    libjasper-dev \
 && make clean ; make

#This container is used to run our FLEXPART model:
FROM debian/tools:main AS runtime
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"

RUN apt-get update && apt-get install -y \
    bc \
    libgrib-api-dev \
 && echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list \
 && apt-get update && apt-get install -y \
    libjasper-dev
COPY flexpart_model /flexpart_model
WORKDIR /flexpart_model
COPY --from=builder /flexpart_code/flexpart.gfs /flexpart_model/flexpart.gfs

ENV PATH="${PATH}:/flexpart_model" \
    OMP_STACKSIZE="512M" \
    INPUT_DATA_MOUNT_POINT="/flexpart_input" \
    OUTPUT_DATA_MOUNT_POINT="/flexpart_output" \
    RUN_NAME="flexpart_run" \
    DIRECTION="-1" \
    START_DATE="2016-09-19" \
    START_HOUR="00" \
    RUN_LENGTH="3" \
    LATITUDE="47.5" \
    LONGITUDE="11"
ENTRYPOINT ["flexpart.sh"]
CMD ["flexpart.gfs"]

#This container is used to compile FLEXPART:
FROM debian:stretch AS builder
RUN apt-get update && apt-get install -y \
    gfortran=4:6.3.0-4 \
    libgrib-api-dev=1.19.0-1 \
    build-essential=12.3 \
 && echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list \
 && apt-get update && apt-get install -y \
    libjasper-dev=1.900.1-debian1-2.4+deb8u3
WORKDIR /flexpart_code
COPY flexpart_code /flexpart_code/
RUN make clean ; make
COPY flexpart_model /flexpart_model/

#This container is used to run our FLEXPART model:
FROM debian:stretch AS runtime
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"
RUN apt-get update && apt-get install -y \
    bc=1.06.95-9+b3 \
    libgrib-api-dev=1.19.0-1 \
 && echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list \
 && apt-get update && apt-get install -y \
    libjasper-dev=1.900.1-debian1-2.4+deb8u3 \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /flexpart_model
COPY --from=builder /flexpart_model /flexpart_code/flexpart.gfs /flexpart_model/
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

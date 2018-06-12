FROM debian/base:main AS builder
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"

ADD flexpart_code /flexpart_code
WORKDIR /flexpart_code
RUN apt-get -y install libgrib-api-dev && \
    echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install libjasper-dev && \
    apt-get -y install gfortran && \
    apt-get -y install build-essential && \
    make clean ; make

FROM debian/base:main AS runtime

ADD flexpart_model /flexpart_model
WORKDIR /flexpart_model
RUN apt-get -y install libgrib-api-dev && \
    echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install libjasper-dev
COPY --from=builder /flexpart_code/flexpart.gfs /flexpart_model/flexpart.gfs

CMD ["flexpart.sh"]

# This container is used to compile FLEXPART:
FROM debian/base:main AS builder
ADD flexpart_code /flexpart_code
WORKDIR /flexpart_code
RUN apt-get -y install libgrib-api-dev && \
    echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install libjasper-dev && \
    apt-get -y install gfortran && \
    apt-get -y install build-essential && \
    make clean ; make

# This container is used to run our FLEXPART model:
FROM debian/tools:main AS runtime
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"
ENV PATH="${PATH}:/flexpart_model"

COPY flexpart_model /flexpart_model
WORKDIR /flexpart_model
COPY --from=builder /flexpart_code/flexpart.gfs /flexpart_model/flexpart.gfs
RUN apt-get -y install libgrib-api-dev && \
    echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install libjasper-dev

ENTRYPOINT ["flexpart.sh"]
CMD ["flexpart.gfs"]

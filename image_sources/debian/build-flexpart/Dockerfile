FROM debian/base
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"

ADD custom_flexpart_code /custom_flexpart_code
WORKDIR /custom_flexpart_code
RUN apt-get -y install libgrib-api-dev && \
    echo "deb http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list && \
    apt-get update && \
    apt-get -y install libjasper-dev && \
    apt-get -y install gfortran && \
    apt-get -y install build-essential && \
    make clean ; make

CMD ["/bin/bash"]

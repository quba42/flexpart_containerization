FROM flexpart/custom_build
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"

ADD flexpart_example /flexpart_example
WORKDIR /flexpart_example
RUN cp /custom_flexpart_code/flexpart.gfs .

CMD ["/flexpart_example/flexpart.sh"]

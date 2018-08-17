FROM debian:jessie
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"

#Install quicklook-gif dependencies:
RUN apt-get update && apt-get install -y \
    python=2.7.9-1 \
    python-numpy=1:1.8.2-2 \
    python-matplotlib=1.4.2-3.1 \
    python-mpltoolkits.basemap=1.0.7+dfsg-1 \
    imagemagick=8:6.8.9.9-5+*
WORKDIR /quicklook
COPY quicklook.sh sources/src /quicklook/
ENV PATH="${PATH}:/quicklook"
ENTRYPOINT ["quicklook.sh", "quick_look.py"]
CMD ["--help"]

FROM debian:jessie
LABEL maintainer="Quirin Pamp <q.pamp.2009@my.bristol.ac.uk>"

#Install quicklook-gif dependencies:
RUN apt-get update && apt-get install -y \
    python=2.7.9-1 \
    python-numpy=1:1.8.2-2 \
    python-matplotlib=1.4.2-3.1 \
    python-mpltoolkits.basemap=1.0.7+dfsg-1 \
    imagemagick=8:6.8.9.9-5+*
WORKDIR /quicklook
COPY quicklook.sh set_matplotlib_backend.sh sources/src /quicklook/
ENV PATH="${PATH}:/quicklook" \
    FLEXPART_OUTPUT_MOUNT_POINT="/flexpart_output" \
    FLEXPART_OUTPUT_FOLDER="" \
    LOWER_LEFT_LONGITUDE="-20" \
    LOWER_LEFT_LATITUDE="30" \
    UPPER_RIGHT_LONGITUDE="40" \
    UPPER_RIGHT_LATITUDE="65" \
    MAXIMUM_GRID="FALSE" \
    LAYERS="0 5" \
    DOMAIN_TYPE="mother" \
    TIME_INTEGRATION="not yet implemented" \
    IMAGE_OUTPUT_PATH="not yet implemented" \
    RECEPTORS_FILE="not yet implemented" \
    DATA_FACTOR="not yet implemented" \
    UNIT_LABEL="not yet implemented" \
    OUTPUT_FILE_NAME="not yet implemented" \
    TITLE="not yet implemented" \
    PROJECTION="cyl" \
    SPECIES="1" \
    AGE_CLASS="not yet implemented" \
    CONCENTRATIONS="not yet implemented" \
    DRY_DEPO="not yet implemented" \
    WET_DEPO="not yet implemented"
ENTRYPOINT ["quicklook.sh"]
CMD ["-h"]

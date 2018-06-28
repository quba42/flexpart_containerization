FROM debian:stretch
LABEL maintainer="Quirin Pamp <pampq@cip.ifi.lmu.de>"

#Install additional debian tools for interactive containers:
RUN apt-get update && apt-get install -y \
    less \
    ranger \
    vim

CMD ["bash"]

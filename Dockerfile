FROM ubuntu:21.04
#Install all needed libraries
RUN apt update && apt upgrade -y \
  && apt install libsodium-dev -y \
  && apt install libnuma-dev -y \
  && apt install libffi-dev -y \
  && apt install libsnappy-dev -y \
  && apt install libleveldb-dev -y \
  && apt install curl -y;

#Install previsous versions of libffi libs
RUN curl -LO http://archive.ubuntu.com/ubuntu/pool/main/libf/libffi/libffi6_3.2.1-8_amd64.deb \
    && dpkg -i libffi6_3.2.1-8_amd64.deb;
RUN apt install libffi6 libffi7 -y 

# Preparing binary to run in container. 
WORKDIR /cardano-faucet
COPY temp-build/cardano-faucet-exe /cardano-faucet/
COPY config/config.dhall /etc/cardano-faucet/
EXPOSE 8083
ENTRYPOINT /cardano-faucet/cardano-faucet-exe $0
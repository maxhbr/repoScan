FROM fpco/stack-build:lts-12.10 as build
RUN mkdir /opt/build \
 && mkdir /opt/build/src \
 && echo "module Main where" > /opt/build/src/Main.hs \
 && echo "main = putStrLn \"to be overwritten\"" >> /opt/build/src/Main.hs
COPY LICENSE repoScan.cabal Setup.hs stack.yaml /opt/build/
RUN cd /opt/build && stack build --system-ghc
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:18.04
RUN apt-get update && apt-get install -y \
  ca-certificates netbase git
RUN mkdir -p /opt/repoScan
ARG BINARY_PATH
WORKDIR /opt/repoScan
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.10/8.4.3/bin .
CMD ["/opt/repoScan/repoScan"]

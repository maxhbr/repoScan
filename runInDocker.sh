#!/usr/bin/env bash
# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -ex

if [ $# -ne 3 ]; then
    exit 1
fi

workdir=$(pwd)/workdir
mkdir -p $workdir
cd $(dirname "${BASH_SOURCE[0]}")

docker build -t mhuber/reposcan .
docker run -it --net=host \
       -v "$workdir:/workdir" \
       mhuber/reposcan \
       /opt/repoScan/repoScan $@ /workdir

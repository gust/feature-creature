#!/usr/bin/env bash

# USAGE:
# source ./script/development/start-all-servers.sh

${FC_SERVER_PROJECT_ROOT}/dist/build/feature-creature-web/feature-creature-web&
cd ${FC_WEB_PROJECT_ROOT}
(elm reactor)&
cd -

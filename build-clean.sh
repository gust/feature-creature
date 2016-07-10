#!/bin/bash

cd users-service-client && stack clean && stack build --copy-bins && cd - && \
cd users-service && stack clean && stack build --copy-bins && cd - && \
cd auth-service && stack clean && stack build --copy-bins && cd - && \
cd marketing-site && stack clean && stack build --copy-bins && cd -

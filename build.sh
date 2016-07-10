#!/bin/bash

cd users-service-client && stack build --copy-bins && cd - && \
cd users-service && stack build --copy-bins && cd - && \
cd auth-service && stack build --copy-bins && cd - && \
cd marketing-site && stack build --copy-bins && cd -

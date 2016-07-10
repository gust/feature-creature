#!/bin/bash

cd users-service-client && stack build --test && cd - && \
cd users-service && stack build --test && cd - && \
cd auth-service && stack build --test && cd - && \
cd marketing-site && stack build --test && cd -

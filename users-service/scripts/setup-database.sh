#!/bin/bash

createuser -s -e users_service
createdb -e users_service

createuser -s -e users_service_test
createdb -e users_service_test

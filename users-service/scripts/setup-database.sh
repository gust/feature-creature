#!/bin/bash

FC_USERS_SERVICE_USER=fc_users_service
FC_USERS_SERVICE_DB_NAME=fc_users_service
FC_USERS_SERVICE_TEST_DB_NAME=fc_users_service_test

docker run -it --rm postgres createuser -h 172.17.0.1 -U featurecreature -e $FC_USERS_SERVICE_USER
docker run -it --rm postgres createdb -h 172.17.0.1 -U featurecreature -e --owner=$FC_USERS_SERVICE_USER $FC_USERS_SERVICE_DB_NAME
docker run -it --rm postgres createdb -h 172.17.0.1 -U featurecreature -e --owner=$FC_USERS_SERVICE_USER $FC_USERS_SERVICE_TEST_DB_NAME

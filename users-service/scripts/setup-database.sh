#!/bin/bash

FC_USERS_SERVICE_USER=fc_users_service
FC_USERS_SERVICE_DB_NAME=fc_users_service
FC_USERS_SERVICE_TEST_DB_NAME=fc_users_service_test

createuser -e $FC_USERS_SERVICE_USER
createdb -e --owner=$FC_USERS_SERVICE_USER $FC_USERS_SERVICE_DB_NAME
createdb -e --owner=$FC_USERS_SERVICE_USER $FC_USERS_SERVICE_TEST_DB_NAME

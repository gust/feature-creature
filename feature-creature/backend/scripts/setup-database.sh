#!/bin/bash

FC_APP_USER=fc_feature_creature
FC_APP_DB_NAME=fc_feature_creature
FC_APP_TEST_DB_NAME=fc_feature_creature_test

docker run -it --rm postgres createuser -h 172.17.0.1 -U featurecreature -e $FC_APP_USER
docker run -it --rm postgres createdb -h 172.17.0.1 -U featurecreature -e --owner=$FC_APP_USER $FC_APP_DB_NAME
docker run -it --rm postgres createdb -h 172.17.0.1 -U featurecreature -e --owner=$FC_APP_USER $FC_APP_TEST_DB_NAME

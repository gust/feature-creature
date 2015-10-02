#!/usr/bin/env bash

# USAGE:
# source ./script/development/bootstrap-env.sh

FC_DB_NAME="feature_creature"
FC_DB_HOST="localhost"
FC_DB_PORT="5432"
FC_DB_USER=$(whoami)

echo "Setting FC_DB_NAME environment variable:" $FC_DB_NAME
export FC_DB_NAME=$FC_DB_NAME
echo "Setting FC_DB_HOST environment variable..." $FC_DB_HOST
export FC_DB_HOST=$FC_DB_HOST
echo "Setting FC_DB_PORT environment variable..." $FC_DB_PORT
export FC_DB_PORT=$FC_DB_PORT
echo "Setting FC_DB_USER environment variable..." $FC_DB_USER
export FC_DB_USER=$FC_DB_USER

echo

echo "Attempting to create database user:" $FC_DB_USER
createuser -s -e $FC_DB_USER

echo

echo "Attempting to create database:" $FC_DB_NAME
createdb -e $FC_DB_NAME

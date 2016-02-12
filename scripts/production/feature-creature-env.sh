#!/usr/bin/env bash

# USAGE:
# source ./script/production/feature-creature-env.sh

FC_API_PATH="http://www.feature-creature.io"
FC_DB_NAME="feature_creature"
FC_DB_HOST="localhost"
FC_DB_PORT="5432"
FC_DB_USER=$(whoami)
FC_DB_PASS=""
FC_DB_POOL_SIZE=1
FC_SERVER_PROJECT_ROOT="$HOME/feature-creature"
FC_WEB_PROJECT_ROOT="$HOME/feature-creature-client"
FC_DATA_FILES_PATH="$FC_SERVER_PROJECT_ROOT/.app-data"

FC_AWS_SQS_URL="https://sqs.us-east-1.amazonaws.com/675495447720/feature-creature"
FC_SEARCH_SERVICE_ROOT="$HOME/search-service"
FC_ELASTIC_SEARCH_URL="http://localhost:9200"
FC_ELASTIC_SEARCH_INDEX_NAME="feature-creature"

echo "Setting FC_DB_NAME environment variable:" $FC_DB_NAME
export FC_DB_NAME=$FC_DB_NAME

echo "Setting FC_DB_HOST environment variable..." $FC_DB_HOST
export FC_DB_HOST=$FC_DB_HOST

echo "Setting FC_DB_PORT environment variable..." $FC_DB_PORT
export FC_DB_PORT=$FC_DB_PORT

echo "Setting FC_DB_USER environment variable..." $FC_DB_USER
export FC_DB_USER=$FC_DB_USER

echo "Setting FC_DB_PASS environment variable..." $FC_DB_PASS
export FC_DB_PASS=$FC_DB_PASS

echo "Setting FC_DB_POOL_SIZE environment variable..." $FC_DB_POOL_SIZE
export FC_DB_POOL_SIZE=$FC_DB_POOL_SIZE

echo "Setting FC_DATA_FILES_PATH environment variable..." $FC_DATA_FILES_PATH
export FC_DATA_FILES_PATH=$FC_DATA_FILES_PATH

echo "Setting FC_SERVER_PROJECT_ROOT environment variable..." $FC_SERVER_PROJECT_ROOT
export FC_SERVER_PROJECT_ROOT=$FC_SERVER_PROJECT_ROOT

echo "Setting FC_WEB_PROJECT_ROOT environment variable..." $FC_WEB_PROJECT_ROOT
export FC_WEB_PROJECT_ROOT=$FC_WEB_PROJECT_ROOT

echo "Setting FC_AWS_SQS_URL environment variable:" $FC_AWS_SQS_URL
export FC_AWS_SQS_URL=$FC_AWS_SQS_URL

echo "Setting FC_SEARCH_SERVICE_ROOT environment variable:" $FC_SEARCH_SERVICE_ROOT
export FC_SEARCH_SERVICE_ROOT=$FC_SEARCH_SERVICE_ROOT

echo "Setting FC_ELASTIC_SEARCH_URL environment variable:" $FC_ELASTIC_SEARCH_URL
export FC_ELASTIC_SEARCH_URL=$FC_ELASTIC_SEARCH_URL

echo "Setting FC_ELASTIC_SEARCH_INDEX_NAME environment variable: " $FC_ELASTIC_SEARCH_INDEX_NAME
export FC_ELASTIC_SEARCH_INDEX_NAME=$FC_ELASTIC_SEARCH_INDEX_NAME

echo

echo "Attempting to create database user:" $FC_DB_USER
sudo -u postgres createuser -s -e $FC_DB_USER

echo

echo "Attempting to create database:" $FC_DB_NAME
createdb -e $FC_DB_NAME

echo "Attempting to create data directory:" $FC_DATA_FILES_PATH
mkdir -p $FC_DATA_FILES_PATH

echo "Attempting to create Elastic Search index:" $FC_ELASTIC_SEARCH_INDEX_NAME
curl -XPUT "http://localhost:9200/"$FC_ELASTIC_SEARCH_INDEX_NAME
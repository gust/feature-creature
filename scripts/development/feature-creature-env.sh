#!/usr/bin/env bash

# USAGE:
# source ./script/development/feature-creature-env.sh

FC_API_PATH="http://localhost:8081"
FC_FEATURES_API="http://localhost:8082"

FC_DB_NAME="feature_creature"
FC_DB_HOST="localhost"
FC_DB_PORT="5432"
FC_DB_USER=$(whoami)
FC_DB_POOL_SIZE=1
FC_SERVER_PROJECT_ROOT="$HOME/workspace/feature-creature"
FC_WEB_PROJECT_ROOT="$HOME/workspace/feature-creature-client"
FC_DATA_FILES_PATH="$FC_SERVER_PROJECT_ROOT/.app-data"

FC_RABBITMQ_HOST="127.0.0.1"
FC_RABBITMQ_PATH="/"
FC_RABBITMQ_USER="guest"
FC_RABBITMQ_PASS="guest"
FC_RABBITMQ_EXCHANGE_NAME="feature-creature"

FC_SEARCH_SERVICE_ROOT="$HOME/workspace/search-service"
FC_ELASTIC_SEARCH_URL="http://localhost:9200"
FC_ELASTIC_SEARCH_INDEX_NAME="feature-creature"
FC_ELASTIC_SEARCH_SHARD_COUNT=1
FC_ELASTIC_SEARCH_REPLICA_COUNT=0

echo "Setting FC_DB_NAME environment variable:" $FC_DB_NAME
export FC_DB_NAME=$FC_DB_NAME

echo "Setting FC_DB_HOST environment variable..." $FC_DB_HOST
export FC_DB_HOST=$FC_DB_HOST

echo "Setting FC_DB_PORT environment variable..." $FC_DB_PORT
export FC_DB_PORT=$FC_DB_PORT

echo "Setting FC_DB_USER environment variable..." $FC_DB_USER
export FC_DB_USER=$FC_DB_USER

echo "Setting FC_DB_POOL_SIZE environment variable..." $FC_DB_POOL_SIZE
export FC_DB_POOL_SIZE=$FC_DB_POOL_SIZE

echo "Setting FC_DATA_FILES_PATH environment variable..." $FC_DATA_FILES_PATH
export FC_DATA_FILES_PATH=$FC_DATA_FILES_PATH

echo "Setting FC_SERVER_PROJECT_ROOT environment variable..." $FC_SERVER_PROJECT_ROOT
export FC_SERVER_PROJECT_ROOT=$FC_SERVER_PROJECT_ROOT

echo "Setting FC_WEB_PROJECT_ROOT environment variable..." $FC_WEB_PROJECT_ROOT
export FC_WEB_PROJECT_ROOT=$FC_WEB_PROJECT_ROOT

echo "Setting FC_SEARCH_SERVICE_ROOT environment variable:" $FC_SEARCH_SERVICE_ROOT
export FC_SEARCH_SERVICE_ROOT=$FC_SEARCH_SERVICE_ROOT

echo "Setting FC_ELASTIC_SEARCH_URL environment variable:" $FC_ELASTIC_SEARCH_URL
export FC_ELASTIC_SEARCH_URL=$FC_ELASTIC_SEARCH_URL

echo "Setting FC_ELASTIC_SEARCH_INDEX_NAME environment variable: " $FC_ELASTIC_SEARCH_INDEX_NAME
export FC_ELASTIC_SEARCH_INDEX_NAME=$FC_ELASTIC_SEARCH_INDEX_NAME

echo "Setting FC_ELASTIC_SEARCH_SHARD_COUNT environment variable: " $FC_ELASTIC_SEARCH_INDEX_NAME
export FC_ELASTIC_SEARCH_SHARD_COUNT=$FC_ELASTIC_SEARCH_SHARD_COUNT

echo "Setting FC_ELASTIC_SEARCH_REPLICA_COUNT environment variable: " $FC_ELASTIC_SEARCH_INDEX_NAME
export FC_ELASTIC_SEARCH_REPLICA_COUNT=$FC_ELASTIC_SEARCH_REPLICA_COUNT

echo "Setting FC_RABBITMQ_HOST environment variable: " $FC_RABBITMQ_HOST
export FC_RABBITMQ_HOST=$FC_RABBITMQ_HOST

echo "Setting FC_RABBITMQ_PATH environment variable: " $FC_RABBITMQ_PATH
export FC_RABBITMQ_PATH=$FC_RABBITMQ_PATH

echo "Setting FC_RABBITMQ_USER environment variable: " $FC_RABBITMQ_USER
export FC_RABBITMQ_USER=$FC_RABBITMQ_USER

echo "Setting FC_RABBITMQ_PASS environment variable: " $FC_RABBITMQ_PASS
export FC_RABBITMQ_PASS=$FC_RABBITMQ_PASS

echo "Setting FC_RABBITMQ_EXCHANGE_NAME environment variable: " $FC_RABBITMQ_EXCHANGE_NAME
export FC_RABBITMQ_EXCHANGE_NAME=$FC_RABBITMQ_EXCHANGE_NAME

echo "Setting FC_FEATURES_API environment variable: " $FC_FEATURES_API
export FC_FEATURES_API=$FC_FEATURES_API

echo

echo "Attempting to create database user:" $FC_DB_USER
createuser -s -e $FC_DB_USER

echo

echo "Attempting to create database:" $FC_DB_NAME
createdb -e $FC_DB_NAME

echo "Attempting to create data directory:" $FC_DATA_FILES_PATH
mkdir -p $FC_DATA_FILES_PATH

echo "Attempting to create Elastic Search index:" $FC_ELASTIC_SEARCH_INDEX_NAME
curl -XPUT "http://localhost:9200/"$FC_ELASTIC_SEARCH_INDEX_NAME

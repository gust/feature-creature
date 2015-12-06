#!/usr/bin/env bash

# USAGE:
# source ./script/development/search-service-env.sh

FC_SEARCH_SERVICE_ROOT="$HOME/workspace/search-service"
FC_ELASTIC_SEARCH_URL="http://localhost:9200"


echo "Setting FC_SEARCH_SERVICE_ROOT environment variable:" $FC_SEARCH_SERVICE_ROOT
export FC_SEARCH_SERVICE_ROOT=$FC_SEARCH_SERVICE_ROOT

echo "Setting FC_ELASTIC_SEARCH_URL environment variable:" $FC_ELASTIC_SEARCH_URL
export FC_ELASTIC_SEARCH_URL=$FC_ELASTIC_SEARCH_URL

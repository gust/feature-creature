#!/usr/bin/env bash

# USAGE:
# source ./script/development/search-service-env.sh

FC_AWS_ELASTIC_SEARCH_URL="http://localhost:9200"
FC_AWS_SQS_URL="https://sqs.us-east-1.amazonaws.com/675495447720/feature-creature"
FC_SEARCH_SERVICE_ROOT="$HOME/workspace/search-service"


echo "Setting FC_AWS_ELASTIC_SEARCH_URL environment variable:" $FC_AWS_ELASTIC_SEARCH_URL
export FC_AWS_ELASTIC_SEARCH_URL=$FC_AWS_ELASTIC_SEARCH_URL

echo "Setting FC_AWS_SQS_URL environment variable:" $FC_AWS_SQS_URL
export FC_AWS_SQS_URL=$FC_AWS_SQS_URL

echo "Setting FC_SEARCH_SERVICE_ROOT environment variable:" $FC_SEARCH_SERVICE_ROOT
export FC_SEARCH_SERVICE_ROOT=$FC_SEARCH_SERVICE_ROOT

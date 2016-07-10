#!/bin/bash

# Copy .env files to the application data directories
mkdir -p $HOME/.fc-marketing-site
cp marketing-site/env/development.env $HOME/.fc-marketing-site

mkdir -p $HOME/.fc-auth-service
cp auth-service/env/development.env $HOME/.fc-auth-service

mkdir -p $HOME/.fc-users-service
cp users-service/env/development.env $HOME/.fc-users-service

foreman start -f Procfile.local

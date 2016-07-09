#!/bin/bash

cd lib && stack build --copy-bins && cd -
cd exe-feature-creature-api && stack build --copy-bins && cd -
cd exe-features-api && stack build --copy-bins && cd -
cd exe-repo-puller && stack build --copy-bins && cd -
cd exe-repo-refresher && stack build --copy-bins && cd -
cd exe-search-service && stack build --copy-bins && cd -

#!/bin/bash

cd lib && stack clean && stack build --copy-bins && cd -
cd exe-feature-creature-api && stack clean && stack build --copy-bins && cd -
cd exe-features-api && stack clean && stack build --copy-bins && cd -
cd exe-repo-puller && stack clean && stack build --copy-bins && cd -
cd exe-repo-refresher && stack clean && stack build --copy-bins && cd -
cd exe-search-service && stack clean && stack build --copy-bins && cd -

#!/bin/bash

find . -type f -name '*.env' -print0 2>/dev/null | xargs -0 blackbox_register_new_file

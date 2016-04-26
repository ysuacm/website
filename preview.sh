#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"

rm -rf "$cwd/_site/" "$cwd/_cache/"

if [ ! -f "$cwd/dist/build/ysuacm/ysuacm" ]; then
  echo "ysuacm binary not found."
  exit 1
fi

"$cwd/dist/build/ysuacm/ysuacm" watch

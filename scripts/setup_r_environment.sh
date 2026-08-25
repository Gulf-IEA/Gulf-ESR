#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
  if command -v sudo >/dev/null 2>&1; then
    sudo apt-get update
    sudo apt-get install -y r-base
  else
    apt-get update
    apt-get install -y r-base
  fi
fi

Rscript --vanilla "${SCRIPT_DIR}/setup_r_packages.R"

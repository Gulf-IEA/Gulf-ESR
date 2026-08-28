#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if command -v apt-get >/dev/null 2>&1; then
  if command -v sudo >/dev/null 2>&1; then
    sudo apt-get update
    sudo apt-get install -y \
      r-base \
      r-cran-dplyr \
      r-cran-httr \
      r-cran-lubridate \
      r-cran-tidyr
  else
    apt-get update
    apt-get install -y \
      r-base \
      r-cran-dplyr \
      r-cran-httr \
      r-cran-lubridate \
      r-cran-tidyr
  fi
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript is unavailable; install R before continuing." >&2
  exit 1
fi

Rscript --vanilla "${SCRIPT_DIR}/setup_r_packages.R"

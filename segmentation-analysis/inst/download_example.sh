#!/bin/bash

JSON_FILE_ID="1fevP-Ex6wJcL_2xqzA7EaXPjTT4opJr1"
SVS_FILE_ID="1DT1cY3_vRlJwzr-fEv031uXS0nS7I-OD"

JSON_OUTPUT="data/TCGA-02-0001-01Z-00-DX1.83fce43e-42ac-4dcd-b156-2908e75f2e47.json"
SVS_OUTPUT="data/TCGA-02-0001-01Z-00-DX1.83fce43e-42ac-4dcd-b156-2908e75f2e47.svs"

# gdown
if ! command -v gdown &> /dev/null; then
  echo "You need gdown: pip install gdown"
  exit 1
fi

# Create output directory if needed
mkdir -p data

# Download JSON
gdown "$JSON_FILE_ID" -O "$JSON_OUTPUT"

# Download SVS
gdown "$SVS_FILE_ID" -O "$SVS_OUTPUT"

echo "Download complete!"

#!/bin/bash

FILE_ID="1fevP-Ex6wJcL_2xqzA7EaXPjTT4opJr1"
OUTPUT="data/TCGA-02-0001-01Z-00-DX1.83fce43e-42ac-4dcd-b156-2908e75f2e47.json"

# gdown
if ! command -v gdown &> /dev/null; then
echo "You need gdown: pip install gdown"
exit 1
fi

# Download file
gdown "$FILE_ID" -O "$OUTPUT"

echo "Download complete!"

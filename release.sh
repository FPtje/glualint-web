#!/usr/bin/env bash

# Pushes the latest glualint-web build to the gh-pages branch, as well as
# pushing to cachix.

set -o errexit \
    -o nounset \
    -o pipefail

if [ -z ${CACHIX_AUTH_TOKEN+x} ]; then
    echo "Please set the CACHIX_AUTH_TOKEN"
    exit 1
fi;

DRV_PATH=$(nix-instantiate --no-gc-warning)

OUT_PATHS=$(nix build "$DRV_PATH^*" --out-link glualint-web-result --print-build-logs --print-out-paths)

nix-store --query --requisites --include-outputs "$DRV_PATH" | grep -v '\.drv' | cachix push glualint
echo "$OUT_PATHS" | cachix push glualint

TMP_DIR=$(mktemp -d)

git clone --branch gh-pages git@github.com:FPtje/glualint-web.git --depth=1 "$TMP_DIR"

OUTPUT_PATH="./glualint-web-result/bin/glualint-web.jsexe"

cp --remove-destination --target-directory="$TMP_DIR" \
    "$OUTPUT_PATH/all.js" \
    "$OUTPUT_PATH/lib.js" \
    "$OUTPUT_PATH/out.js" \
    "$OUTPUT_PATH/rts.js" \
    "$OUTPUT_PATH/runmain.js" \
    "$OUTPUT_PATH/styles.css"

pushd "$TMP_DIR"

git add ./*.js ./*.css
git commit -m "Update glualint-web" || true
git push

popd

rm -rf ./glualint-web-result*
rm -rf "$TMP_DIR"

#!/usr/bin/env sh

set -o errexit

stack haddock
rev=$(git rev-parse HEAD)

tmp=$(mktemp -d)
cleanup() {
    rm -rf "$tmp"
}
trap cleanup EXIT

git clone git@github.com:utdemir/apidoc-hs \
    --branch gh-pages --single-branch --depth 1 \
    "$tmp"

cp -r "$(stack path --local-doc-root)"/* "$tmp"

set -o xtrace

cd "$tmp"

git add .
git commit -m "Update haddock to $rev"
git remote add origin git@github.com:utdemir/apidoc-hs
git push --force --set-upstream origin gh-pages

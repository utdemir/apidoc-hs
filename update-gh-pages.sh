#!/usr/bin/env sh

set -o errexit

stack haddock

tmp=$(mktemp -d)
cleanup() {
    rm -rf "$tmp"
}
trap cleanup EXIT

cp -r "$(stack path --local-doc-root)"/* "$tmp"

set -o xtrace

cd "$tmp"

git init
git checkout -b gh-pages
git add .
git commit -m "Add haddock"
git remote add origin git@github.com:utdemir/apidoc-hs
git push --force --set-upstream origin gh-pages

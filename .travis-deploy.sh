#!/bin/bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

HTML_DIR=$(make echo-html-dir)

# DEPLOY_KEY was generated and set using:
#   $ ssh-keygen -b 4096 -N '' -C '' -f deploy-key
#   $ travis env set DEPLOY_KEY -- "$(cat deploy-key)"
eval $(ssh-agent -s)
ssh-add <(set +x; echo "$DEPLOY_KEY") # set +x prevents leaking $DEPLOY_KEY

git clone git@github.com:hazelgrove/build.git
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Push From Travis"

cd build

if [ -d "$TRAVIS_BRANCH" ]; then
  rm -rf "$TRAVIS_BRANCH"
fi

mkdir "$TRAVIS_BRANCH"
cp -r ../"$HTML_DIR"/* "$TRAVIS_BRANCH"

git add .
if git commit -m "Travis Build"; then
  git push origin master
fi


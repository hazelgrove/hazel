#!/bin/bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

HTML_DIR=$(make echo-html-dir)

echo "set up private key to push to hazelgrove/build"
# DEPLOY_KEY was generated using:
# $ travis env set DEPLOY_KEY "$(perl -pe 's/\n/\\n/g;' <deploy-key-file)"
# $ travis env set DEPLOY_KEY -- "$(cat deploy-key-file)"
# So `echo -n -e` will do the proper translation
eval `ssh-agent -s`
#set +x # Prevent echoing of $DEPLOY_KEY
ssh-add <(echo "$DEPLOY_KEY")
#set -x

echo "clone hazelgrove/build"
git clone git@github.com:hazelgrove/build.git
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Push From Travis"
cd build

echo "clear contents of subdir $TRAVIS_BRANCH"
if [ -d "$TRAVIS_BRANCH" ]
then
  echo "subdir found, clearing contents"
  rm -rf "$TRAVIS_BRANCH"
fi
  echo "subdir not found, creating new"
  mkdir "$TRAVIS_BRANCH"

cp -r ../"$HTML_DIR"/* "$TRAVIS_BRANCH"

git add .
if git commit -m "Travis Build"; then
  git push origin master
fi

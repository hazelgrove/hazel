#!/bin/bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

make release
HTML_DIR=$(make echo-html-dir)

# cf.
# http://markbucciarelli.com/posts/2019-01-26_how-to-push-to-github-from-travis-ci.html
# https://stackoverflow.com/questions/18935539/authenticate-with-github-using-a-token/22977235#22977235
echo "set up private key to push to hazel-build"
set +x # Prevent echoing of $travis_key_password
openssl aes-256-cbc -k "$travis_key_password" -d -md sha256 -a -in hazel-build-key.enc -out hazel-build-key
set -x
chmod 600 hazel-build-key
eval `ssh-agent -s`
ssh-add hazel-build-key

git clone git@github.com:hazelgrove/build.git
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Push From Travis"
cd build

echo "clear contents of subdir $TRAVIS_BRANCH"
if [ -d "$TRAVIS_BRANCH" ]
then
  echo "subdir found, clearing contents"
  rm -rf "$TRAVIS_BRANCH"
  mkdir "$TRAVIS_BRANCH"
else
  echo "subdir not found, creating new"
  mkdir "$TRAVIS_BRANCH"
fi
cp -r ../"$HTML_DIR"/* "$TRAVIS_BRANCH"

git add .
if git commit -m "Travis Build"; then
  git push origin master
fi

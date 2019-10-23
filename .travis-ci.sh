echo "build here"

echo "Compiling new static content"
make release
BUILD_WWW=`make build_www`

echo "chmod"
chmod 600 deploy-key
echo "eval"
eval `ssh-agent -s`
echo "ssh"
ssh-add deploy-key


echo "git clone"
git clone git@github.com:hazelgrove/hazel.git
echo "git conf"
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Push From Travis"
echo "move to hazel"
cd hazel
echo "switch to gh-pages"
git checkout gh-pages

echo "clear gh-pages subdir $TRAVIS_BRANCH"
if [ -d "$TRAVIS_BRANCH" ]
then
  echo "subdir found, clearing contents"
  git rm -rf $TRAVIS_BRANCH/*
else
  echo "subdir not found, creating new"
  mkdir "$TRAVIS_BRANCH"
fi
echo "cp new build contents into subdir"
cp -r ../$BUILD_WWW/* $TRAVIS_BRANCH

echo "git add"
git add .
echo "commit"
git commit -m "Travis Build"
echo "push"
git push origin gh-pages

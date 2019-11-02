echo "build here"
echo "compile new static content"
make release
HTML_DIR=`make echo-html-dir`

# cf.
# http://markbucciarelli.com/posts/2019-01-26_how-to-push-to-github-from-travis-ci.html
# https://stackoverflow.com/questions/18935539/authenticate-with-github-using-a-token/22977235#22977235
echo "set up private key to push to hazel-build"
openssl aes-256-cbc -k "$travis_key_password" -d -md sha256 -a -in hazel-build-key.enc -out hazel-build-key

echo "chmod hazel-build-key"
chmod 600 hazel-build-key
echo "ssh-add hazel-build-key"
eval `ssh-agent -s`
ssh-add hazel-build-key

echo "git clone"
git clone git@github.com:hazelgrove/build.git
echo "git config"
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Push From Travis"
echo "move to hazel-build"
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
echo "cp new build contents into subdir"
cp -r ../"$HTML_DIR"/* "$TRAVIS_BRANCH"

echo "git add"
git add .
echo "git commit"
git commit -m "Travis Build"
echo "git push"
git push origin master

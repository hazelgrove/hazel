echo "build here"

echo "Compiling new static content"
cd ./src
./build.sh

echo "chmod"
chmod 600 ../deploy-key
echo "eval"
eval `ssh-agent -s`
echo "ssh"
ssh-add ../deploy-key


echo "git clone"
git clone git@github.com:hazelgrove/hazel.git
echo "git conf"
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Push From Travis"
echo "move to hazel"
cd hazel
echo "switch to gh-pages"
git checkout gh-pages
echo "get latest files"
cp -r ../www/* ./master/
echo "git add"
git add .
echo "commit"
git commit -m "Travis Build"
echo "push"
git push origin gh-pages

#!/bin/bash

if [ "$TRAVIS_BRANCH" == "master" ]; then
  cd $HOME
  git clone --quiet https://${GH_TOKEN}@github.com/gsnewmark/beatha > /dev/null

  cd beatha
  git remote set-url origin "https://${GH_TOKEN}@github.com/gsnewmark/beatha.git"
  lein2 with-profile prod do clean, compile

  git checkout gh-pages
  git pull origin gh-pages
  cp dev-resources/public/js/beatha.js js/beatha.js
  git add js/beatha.js
  git status
  git commit -m 'Deploy to Github Pages'

  git push -fq origin gh-pages > /dev/null 2>&1
fi

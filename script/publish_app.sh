#!/bin/bash

if [ "$TRAVIS_BRANCH" == "master" ]; then
  cd $HOME
  git clone --quiet https://${GH_TOKEN}@github.com/gsnewmark/beatha > /dev/null

  cd beatha
  lein with-profile prod do clean, compile

  git checkout gh-pages
  cp dev-resources/public/js/beatha.js js/beatha.js
  git add js/beatha.js
  git commit -m 'Deployed to Github Pages'

  git push -fq origin gh-pages > /dev/null
fi

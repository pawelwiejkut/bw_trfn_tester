#!/bin/bash

# Build merged
npm run merge || exit 1
wc -l ./zbw_trfn_tester.abap

# Deploy artifacts
git clone https://github.com/pawelwiejkut/build.git ../build
cp zbw_trfn_tester.abap ../build/zbw_trfn_tester.abap
cd ../build

# Commit
git status
git config user.email "ci@pawelwiejkut.net"
git config user.name "CI"
git add zbw_trfn_tester.abap
git commit -m "CI build " || exit 1
git push -q https://$GITHUB_API_KEY@github.com/pawelwiejkut/build.git > /dev/null 2>&1

#!/bin/bash

# Build merged
npm run merge || exit 1
wc -l ./zbw_trfn_tester.abap

# Deploy artifacts
git clone https://github.com/pawelwiejkut/bw_trfn_tester.git
cp zbw_trfn_tester.abap bw_trfn_tester/last_build/zbw_trfn_tester.abap
cd bw_trfn_tester

# Commit
git status
git config user.email "ci@pawelwiejkut.net"
git config user.name "CI"
git add last_build/zbw_trfn_tester.abap
git commit -m "CI build " || exit 1
git push -q https://$GITHUB_API_KEY@github.com/pawelwiejkut/bw_trfn_tester.git 

#!/bin/bash

sbcl --load bin/compile.lisp

# Package
mv ./lispapp ./lispapp.exe
npm i

# npx prefix needed if electron packager is not installed globally by npm with
# the -g flag
npx electron-packager --overwrite . $APP_NAME

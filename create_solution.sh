#!/bin/sh

if [ -d solution ]; then
    rm -rf solution
fi

mkdir solution
cat $1 | java -jar tosexp.jar | ./gcc.native > solution/lambdaman.gcc

tar -czf ../lambda-llama.tar.gz .

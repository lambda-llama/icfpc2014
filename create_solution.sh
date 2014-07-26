#!/bin/sh

if [ -d solution ]; then
    rm -rf solution
fi

make && make uberjar

mkdir solution
cat $1 | java -jar tosexp.jar | ./gcc.native > solution/lambdaman.gcc

make clean && make clean-uberjar

tar -czf ../lambda-llama.tar.gz .

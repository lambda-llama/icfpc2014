if [[ "$OSTYPE" == "linux-gnu" ]]; then
    pbin="xclip -i -selection clipboard"
else
    pbin="pbcopy"
fi

cat ./strategies/stdlibza.clj $@ | java -jar ./tosexp/target/tosexp-0.1.0-SNAPSHOT.jar | ./gcc.native | $pbin

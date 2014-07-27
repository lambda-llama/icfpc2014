if [[ "$OSTYPE" == "linux-gnu" ]]; then
    pbin="xclip -i -selection clipboard"
else
    pbin="pbcopy"
fi

cat ./strategies/stdlibza.clj $@ | java -jar ./tosexp.jar | ./gcc.native | $pbin

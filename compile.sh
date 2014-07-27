if [[ "$OSTYPE" == "linux-gnu" ]]; then
    pbin="xclip -i -selection clipboard"
else
    pbin="pbcopy"
fi

cat $@ | java -jar ./tosexp.jar | ./gcc.native | $pbin

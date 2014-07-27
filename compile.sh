cat $@ | java -jar ./tosexp.jar | ./gcc.native | xclip -i -selection clipboard

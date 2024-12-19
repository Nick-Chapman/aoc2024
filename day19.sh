tail +3 $1 | egrep "^($(head -1 $1 | sed 's/, /|/g'))*$" | wc -l

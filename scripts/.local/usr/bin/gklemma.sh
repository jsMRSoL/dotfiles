#/bin/bash
query="$1"
LEMMATA="/home/simon/Downloads/diogenes-3.2.0/diogenes/perl/Perseus_Data/greek-lemmata.txt"

query=$( echo $query | sed 's|\([haeiouw]\)\b|\1[()\\/=]\\{0,2\\}|g' )
# echo $query
query=$( echo $query | sed 's|\([haeiouw]\)\([^haeiu\[]\)|\1[()\\/=]\\{0,2\\}\2|g' )
# echo $query
query=$( echo $query | sed 's|\([haeiouw]\)\([^haeiu\[]\)|\1[()\\/=]\\{0,2\\}\2|g' )
# echo $query

sed_query=$( echo $query | sed 's|\\{0,2\\}|*|g' )

# sed_query="s|^\([^[:space:]]*\).*\($sed_query\)\b[[:space:]][^(]*\(([^)]*)*\).*|\1:\2:\3|p"
sed_query="s|^\([^[:space:]]*\).*\($sed_query\)[^a-z()\\/=]*[[:space:]][^(]*\(([^)]*)*\).*|\1:\2:\3|p"
# sed_query="s|^\([^[:space:]]*\).*\($sed_query\)\b*\(\(\ (([^)]*)*\)*\).*|\1:\2:\3|p"

# grep -m 5 '\b'$query'\b' $LEMMATA | cut -f1
grep '\b'$query'\b' $LEMMATA | sed -n "$sed_query" | column -s: -t

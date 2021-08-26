# Define various git-derived latex macros
# (tail is used to slice off the undesired prefixes of the pipelined output)
printf "\\def\\GITDATE {%s}\n"   "$(git log | grep Date | head -n 1 | tail -c +9)"
printf "\\def\\GITVERSION{%s}\n" "$(git log | grep Date | wc -l | tail -c +7)"




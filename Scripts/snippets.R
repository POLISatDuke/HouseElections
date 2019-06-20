### Silly functions to easily convert between years <-> congress numbers
year_to_congress = function(year){
  floor((year - 1789)/2)+1
}

# Returns which congress was in session that year, not which was elected that year
# If an election year is input, function will return the # congress voted in (that year - 1)
congress_to_election = function(congress){
  2 * congress + 1786
}

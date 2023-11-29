
let groups = stdin :split "\n\n" :map |group| { group .lines(clf = false) :map int .sum } .max

groups

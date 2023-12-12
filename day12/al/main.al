
let example_input = "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"

fn solve(input: str) {
  input :trim :lines
    :map |line| {
      let [records, ns] = line :split " "
      let num = arrangements(
        records :split /\.+/ :filter |b| { b },
        ns :split "," :map int,
        "  "
      )

      //print("{line}   =>   {num}")
      num
    }
    :sum
}

fn hash(pieces, ns) {
  (pieces :join ",") + "__" + (ns :join ",")
}

let cache = @{}

fn arrangements(pieces, ns, indent) {
  let h = (pieces, ns)

  if let memoized = cache[h] {
    return memoized + 0
  }

  if ns:len == 0 {
    if pieces :any |p| { "#" :in p } {
      //print("{indent}arrangements({pieces}) {ns} -> IMPOSSIBLE (0)")
      //cache[h] = 0
      return 0
    }
    //print("{indent}arrangements({pieces}) {ns} -> done (1)")
    cache[h] = 1
    return 1
  }

  // find next number
  let (i, n) = ns :enumerate :fold (nil, 0), |(i, n), (j, m)| {
    if m > n { (j, m) } else { (i, n) }
  }

  //print("{indent}arrangements({pieces}) {ns}  check ({i}, {n}) -> {split_around(ns, i)}")
  let (ns_le, ns_ri) = split_around(ns, i)

  let num_possible = pieces
    :enumerate
    :filter |(j, piece)| {
      piece:len >= n
    }
    :map |(j, piece)| {
      let (le, ri) = split_around(pieces, j)
      let num = placements(piece, n)
        :map |(i, p_le, p_ri)| {
          //print("{indent}  place at {i}")
          //print("{indent}    checking left {le + p_le} {ns_le}")
          let num_le = arrangements(
            le + p_le,
            ns_le,
            indent + "      "
          )

          //print("{indent}    checking right {ri + p_ri} {ns_ri}")
          let num_ri = arrangements(
            p_ri + ri,
            ns_ri,
            indent + "      "
          )

          //print("{indent}    {num_le * num_ri}")
          num_le * num_ri
        }
        :sum
      
      //print("{indent}  {num}")
      num
    }
    :sum

  //print("INSERT {h} {indent}{num_possible}")
  if num_possible > 0 { cache[h] = num_possible }
  num_possible
}

fn split_around(arr, i) {
  (
    arr :slice (0, i),
    arr :slice (i + 1)
  )
}

fn placements(piece, n) {
  let s = piece:len - n + 1
  let placements = range(0, s) :filter_map |i| {
    let left_ok = i <= 0 || piece[i-1] == "?"
    let right_ok = i+n >= piece:len || piece[i+n] == "?"

    if left_ok && right_ok {
      let left = if i >= 2 {
        [piece :slice (0, i-1)]
      } else {
        []
      }

      let right = if i+n+2 <= piece:len {
        [piece :slice (i+n+1)]
      } else {
        []
      }

      (i, left, right)
    }
  }

  placements
}

fn bonus(input) {
  let all = input :trim :lines
  let total = all:len
  all
    :enumerate
    :map |(i, line)| {
      let [records, ns] = line :split " "
      let [records, ns] = [
        records + "?" + records + "?" + records + "?" + records + "?" + records,
        ns + "," + ns + "," + ns + "," + ns + "," + ns,
      ]
      let num = arrangements(
        records :split /\.+/ :filter |b| { b },
        ns :split "," :map int,
        "  "
      )

      print("[{i+1}/{total}] {line}   =>   {num}")
      num
    }
    :sum
}

//print(solve("???.### 1,1,3")) // 1
//print(solve(".??..??...?##. 1,1,3")) // 4
print("Example solution: {solve(example_input)}") // 21

//print("Example bonus: {bonus(example_input)}") // 21



//print(solve("????????#?? 7,1"))
//print(solve("?#???##????#.??? 2,4,4,2"))

//print(hash(["?#", "??"], [1, 2, 3]))

// 9281 is too HIGH
// 7269 is too LOW
// ±2.5s
//print(solve(stdin)) // 7490

print("Bonus: {bonus(stdin)}")


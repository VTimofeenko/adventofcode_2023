# Given input.ncl with the shape of:
# { input = m%"<STRING"%m}
# Produce an attrset with the result
let input = (import "./input.ncl").input in
let ParsedNumber = std.contract.Sequence [ Number, std.contract.from_predicate (fun x => x >= 10 && x <= 99)] in
let ResultSchema = std.number.Nat in
let Digits = {
  one = 1,
  two = 2,
  three = 3,
  four = 4,
  five = 5,
  six = 6,
  seven = 7,
  eight = 8,
  nine = 9,
}
in
let calc =
  input
  |> std.string.split "\n"
  |> std.array.map
    (
      fun line =>
        let regex = "(" ++ (std.string.join "|" (["\\d"] @ (std.record.fields Digits))) ++ ")" in
        let numsOnly =
          # Get the first and the last matches of regex
          # This uses regex' greediness to find the last match
          [(std.string.find "%{regex}" line).groups, (std.string.find ".*%{regex}" line).groups]
          |> std.array.flatten
          # Convert "seven" to "7", leave "7" in place
          |> std.array.map (fun stringPart => if std.record.has_field stringPart Digits then std.string.from Digits."%{stringPart}" else stringPart)
        in
        ((std.array.first numsOnly) ++ (std.array.last numsOnly)) |> std.string.to_number
    )
in
{
  _calc | not_exported = calc | Array ParsedNumber,
  result = std.array.reduce_left (+) calc | ResultSchema
}

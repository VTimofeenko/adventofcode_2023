# Given input.ncl with the shape of:
# { input = m%"<STRING"%m}
# Produce an attrset with the result
let input = (import "./input.ncl").input in
let ParsedNumber = std.contract.Sequence [ Number, std.contract.from_predicate (fun x => x >= 10 && x <= 99)] in
let ResultSchema = std.number.Nat in
let calc =
  input
  |> std.string.split "\n"
  |> std.array.map
    (
      fun line =>
        let numsOnly = line |> std.string.characters |> std.array.filter (std.string.is_match "^\\d$")
        in
        ((std.array.first numsOnly) ++ (std.array.last numsOnly)) |> std.string.to_number
    )
in
{
_calc | not_exported = calc | Array ParsedNumber,
result = std.array.reduce_left (+) calc | ResultSchema
}

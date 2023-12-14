let input = import "./input.ncl" in
let ResultSchema = std.number.Nat in
let cubeConstraints : { _ : Number } = {
    red = 12,
    green = 13,
    blue = 14
  }
  in
let
# If a record contains a field -- checks that field agains the constraint
checkExperiment : { _ : Number } -> Bool = fun e => std.record.fields e |> std.array.all (fun field => e."%{field}" <= cubeConstraints."%{field}")
  in
let checkGame : String -> Array { _ : Number } -> Bool = fun _name a => a |> std.array.all checkExperiment
  in
let Games =
  input
  |> std.string.split "\n" # => [ "Game 1: ...", "Game 2: ..." ]
  |> std.array.map
    (
      fun x =>
        # Not sure if " " is guaranteed after ":"
        # This part splits the individual string into a record with a single field "Game X" and an array of valeus
        let parts = std.string.split ": " x
        in
        {
          "%{std.array.first parts}" = (
            std.array.last parts
            |> std.string.split "; "
            |> std.array.map (std.string.split ", ")
            |> std.array.map
              (
                fun innerArray =>
                  innerArray
                  |> std.array.map
                    (
                      fun innerStr =>
                        let innerSplit = std.string.split " " innerStr
                        in
                        { "%{std.array.last innerSplit}" = innerSplit |> std.array.first |> std.string.to_number }
                    )
                  |> std.record.merge_all
              )
          )
        }
    )
  |> std.record.merge_all # => {"Game 1" = [ red = 5, green = 1, blue = 2 ]}
  |> std.record.filter checkGame
  |> std.record.fields
  |> std.array.map (fun game => game |> std.string.split " " |> std.array.last |> std.string.to_number)
in
{
  possibleGames = Games | Array std.number.PosNat,
  result = std.array.reduce_left (+) possibleGames | ResultSchema
}

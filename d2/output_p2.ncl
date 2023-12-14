let input = import "./input.ncl" in
let ResultSchema = std.number.Nat in
let getGamePower = fun game => game |> std.record.values |> std.array.reduce_left (*) in
let getFieldOrNull = fun field record => if std.record.has_field field record then record."%{field}" else null
in
let cubeConstraints : { _ : Number } = {
    red = 12,
    green = 13,
    blue = 14
  }
  in
let findMaxNumber : Array Number -> Number = fun array => std.array.reduce_left std.number.max array in
let findMinimalCubeCount = fun experiments =>
  ["red", "blue", "green"]
  |> std.array.map
    (
      fun color =>
        {
          "%{color}" =
            experiments
            |> std.array.map (getFieldOrNull color)
            |> std.array.filter ((!=) null)
            |> findMaxNumber
        }
    )
  |> std.record.merge_all
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
  |> std.record.map_values findMinimalCubeCount
|> std.record.map_values getGamePower
# |> std.array.map (fun game => game |> std.string.split " " |> std.array.last |> std.string.to_number)
in
{
  processedGames = Games |> std.record.values | Array std.number.PosNat,
  result = std.array.reduce_left (+) processedGames | ResultSchema
}

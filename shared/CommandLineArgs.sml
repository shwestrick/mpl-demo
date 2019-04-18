structure CommandLineArgs =
struct

  fun search key args =
    case args of
      [] => NONE
    | x :: args' =>
        if key = x
        then SOME args'
        else search key args'

  fun parseString key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => Util.die ("Missing argument of \"-" ^ key ^ "\" ")
    | SOME (s :: _) => s

  fun parseInt key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => Util.die ("Missing argument of \"-" ^ key ^ "\" ")
    | SOME (s :: _) =>
        case Int.fromString s of
          NONE => Util.die ("Cannot parse \"-" ^ key ^ "\" " ^ s)
        | SOME x => x

  fun parseBool key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => Util.die ("Missing argument of \"-" ^ key ^ "\" ")
    | SOME ("true" :: _) => true
    | SOME ("false" :: _) => false
    | SOME (s :: _) => Util.die ("Cannot parse \"-" ^ key ^ "\" " ^ s)

end

open Instrumentation

module PLUGIN = struct
  let instrument il fb_bbl bbl =
    il
end

let () =
  plugin := Some (module PLUGIN)
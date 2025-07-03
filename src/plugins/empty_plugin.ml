(*
   This is the instrumentation plugin template.
   It does no instrumentation.
 *)
 open Instrumentation

module PLUGIN = struct
  let instrument il fb_bbl bbl =
    il
end

let () =
  plugin := Some (module PLUGIN)
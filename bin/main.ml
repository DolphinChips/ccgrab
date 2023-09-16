open Cmdliner

let volume =
  let doc = "Specify a specific volume to download (default: download all of them)" in
  Arg.(value & opt (some string) None & info [ "volume" ] ~docv:"VOLUME" ~doc)
;;

let author =
  let doc = "Specify an author of the light novel" in
  Arg.(value & opt string "" & info [ "author" ] ~docv:"AUTHOR" ~doc)
;;

let url =
  let doc = "URL to table of contents of light novel you want to download" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
;;

let ccgrab_t = Term.(const Lwt_main.run $ (const Ccgrab.ccgrab $ volume $ author $ url))

let cmd =
  let info =
    Cmd.info
      "ccgrab"
      ~doc:"Download volumes of LNs in ePub format from cclawtranslations.home.blog"
      ~version:"%%VERSION%%"
  in
  Cmd.v info ccgrab_t
;;

let () = exit @@ Cmd.eval cmd

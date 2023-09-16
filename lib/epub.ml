open Core

type entry =
  { path : string
  ; content : string
  ; mimetype : string
  ; id : string option
  ; properties : string list }

type metadata =
  { identifier : string
  ; title : string
  ; last_modified : string (* i'd prefer to use Core.Time.t for this *)
  ; date : string option
  ; author : string option }

let epub_datetime_of_time time =
  time
  |> Time_float.to_sec_string_with_zone ~zone:Time_float.Zone.utc
  |> String.tr ~target:' ' ~replacement:'T'
;;

let dom_element name ?(attrs = []) children =
  (* Technically you need to put namespace URI into those tuples, but i have no idea why,
     so i don't (: *)
  let name = "", name in
  let attrs = List.map ~f:(fun (name, v) -> ("", name), v) attrs in
  `Element (name, attrs, children)
;;

let generate_content_opf metadata entries spine =
  let date = match metadata.date with
    | Some date -> date
    | None -> Time_float.now () |> epub_datetime_of_time
  in
  let open Markup in
  let e = dom_element in
  (* item element accepts way more attributes, but I just don't need them *)
  let item ~id ~href ~media_type ?properties () =
    let attrs =
      [ "id", id; "href", href; "media-type", media_type ]
      @ (Option.value_map ~default:[] ~f:(fun v -> [ "properties", v ]) properties)
    in
    e "item" ~attrs []
  in
  let author_entry =
      match metadata.author with
      | None -> []
      | Some author ->
        [ e "dc:creator" ~attrs:[ "id", "creator" ] [ `Text author ]
        ; e "meta" [ `Text "aut" ] ~attrs:
          [ "property", "role"
          ; "scheme", "marc:relators"
          ; "refines", "#creator"
          ]
        ]
  in
  let manifest =
    let f entry = match entry.id with
      | None -> None
      | Some id -> begin
          let properties = match entry.properties with
            | [] -> None
            | p -> Some (String.concat ~sep:" " p)
          in
          Some (item ~id ~href:entry.path ~media_type:entry.mimetype ?properties ())
        end
    in
    entries |> List.filter_map ~f
  in
  let spine =
    let f id = e "itemref" ~attrs:[ "idref", id ] [] in
    spine |> List.map ~f
  in
  let xml =
    write_xml
    @@ pretty_print
    @@ from_tree
         Fn.id
         (e
            "package"
            ~attrs:
              [ "unique-identifier", "pub-id"
              ; "version", "3.0"
              ; "xml:lang", "en"
              ; "xmlns", "http://www.idpf.org/2007/opf"
              ; "xmlns:epub", "http://www.idpf.org/2007/ops"
              ]
            [ e
                "metadata"
                ~attrs:[ "xmlns:dc", "http://purl.org/dc/elements/1.1/" ]
                ([ e "dc:identifier" ~attrs:[ "id", "pub-id" ] [ `Text metadata.identifier ]
                ; e "dc:title" [ `Text metadata.title ]
                ; e "dc:language" [ `Text "en" ]
                ; e "dc:date" [ `Text date ]
                ; e
                    "meta"
                    ~attrs:[ "property", "dcterms:modified" ]
                    [ `Text metadata.last_modified ]
                ] @ author_entry)
            ; e "manifest" manifest
            ; e "spine" spine
            ])
    |> to_string
  in
  {xml|<?xml version="1.0" encoding="utf-8"?>|xml} ^ xml
;;

let create filename metadata entries spine =
  let zip = Zip.open_out filename in
  let add_entry ?level content path = Zip.add_entry ?level content zip path in

  add_entry "application/epub+zip" ~level:0 "mimetype";

  let () =
    let container_xml =
      {xml|<?xml version="1.0"?><container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container"><rootfiles><rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/></rootfiles></container>|xml}
    in
    add_entry container_xml "META-INF/container.xml"
  in

  entries |> List.iter ~f:(fun { path; content; _ } -> add_entry content ("OEBPS/" ^ path));

  let () =
    let content_opf = generate_content_opf metadata entries spine in
    add_entry content_opf "OEBPS/content.opf"
  in

  Zip.close_out zip

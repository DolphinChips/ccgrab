(* TODO: this code formatting is not the best, but it is the best i can have
   without spending unreasonable amounts of time fiddling with ocamlformat *)
open Core

type volume =
  { name : string
  ; cover_art_uri : Uri.t
  ; chapters : (string * Uri.t) list
  }

type downloaded_chapter =
  { name : string
  ; normalized_name : string
  ; date : Time_float.t
  ; xhtml_content : string
  ; images : (string * string * string) list (* filename, bytes, mimetype *)
  }

let xml_header = {xml|<?xml version="1.0" encoding="utf-8"?>|xml}

let file_extension_of_mime mime =
  let (_, part) = String.lsplit2_exn mime ~on:'/' in
  match String.lsplit2 part ~on:'+' with
  | Some (_, file_extension) -> file_extension
  | None -> part
;;

let get_body uri =
  let%lwt { headers; _ }, body = Cohttp_lwt_unix.Client.get uri in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let content_type = Cohttp.Header.get headers "Content-Type" in
  Lwt.return (content_type, body)
;;

(* I hope this won't break *)
let get_title_from_toc soup = Soup.(soup $ ".entry-header > h1" |> R.leaf_text |> String.chop_suffix_exn ~suffix:" ToC")

let enumerate_volumes soup =
  Soup.(soup $$ ".entry-content > h2" |> to_list |> List.map ~f:R.leaf_text)
;;

let parse_volume soup volume =
  let open Soup in
  let header_node = soup $ Printf.sprintf {|.entry-content > h2:contains("%s")|} volume in
  let cover_art_node = header_node |> R.previous_element in
  let cover_art_uri =
    cover_art_node
    |> R.child_element
    |> R.child_element
    |> R.attribute "data-orig-file"
    |> Uri.of_string
  in
  let chapters =
    with_stop (fun stop ->
      header_node
      $ " ~ :not(p)"
      |> previous_siblings
      |> elements
      |> fold
           (fun acc e ->
             if String.(e |> name = "p" && e |> R.child_element |> name = "a")
             then (
               let a = e |> R.child_element in
               (a |> R.leaf_text, a |> R.attribute "href" |> Uri.of_string) :: acc)
             else stop.throw acc)
           [])
  in
  { name = volume; cover_art_uri; chapters }
;;

let download_and_prepare_chapter (chapter_name, uri) =
  let%lwt _, chapter_page = get_body uri in
  let open Soup in
  let soup = parse chapter_page in
  (* NOTE: %z exists only in glibc and i don't like how i'm using core_unix to parse this time *)
  let date =
    soup
    $ "time"
    |> R.attribute "datetime"
    |> Time_float_unix.parse ~fmt:"%Y-%m-%dT%H:%M:%S%z" ~zone:Time_float.Zone.utc
  in
  let main_content = soup $ ".entry-content" in
  (* just a helpful function *)
  let delete_nodes nodes = iter delete nodes in
  (* what follows below are various passes that mutate the DOM.
     it might be a good idea to move them to separate functions, but right now there's no need to *)

  (* i'm really afraid that it might filter stuff besides their self-plug,
     but it does filter their self-plug, so it's fine(tm) *)
  main_content $? "h2" |> Option.iter ~f:(fun e -> previous_siblings e |> delete_nodes);
  (* the end of main content is kinda denoted by div[id^="atatags-"], so delete it and its next siblings *)
  (* there might be more than 1 node with this class, but i don't care (: *)
  let atatags = main_content $ {|[id^="atatags-"]|} in
  next_siblings atatags |> delete_nodes;
  delete atatags;
  (* i definitely don't want any tracking in my epub *)
  main_content $$ "script" |> delete_nodes;
  (* it kinda breaks layout, but i don't care *)
  main_content $$ ".wp-block-spacer" |> delete_nodes;
  main_content $$ ".wp-block-separator" |> delete_nodes;
  (* i don't like those single letter names *)
  let image_table =
    (* List operations I use are really inefficient, using any other mutable data structure will be better,
       but also there is like only one gallery per volume and 1 image per chapter, sometimes even no images *)
    let image_table = ref [] in
    let image_counter = ref 1 in
    let extract_image_from_node node =
      let new_node = create_element "img" in
      let number = !image_counter in
      let uri = node |> R.attribute "data-orig-file" |> Uri.of_string in
      image_counter := !image_counter + 1;
      new_node, number, uri
    in
    main_content
    $$ ".wp-block-gallery"
    |> iter (fun gallery ->
         let image_nodes = gallery $$ {|[class^="wp-image-"]|} in
         let container = create_element "div" in
         let images = image_nodes |> to_list |> List.map ~f:(fun image_node ->
           let ((new_node, _, _) as image) = extract_image_from_node image_node in
           append_child container new_node;
           image)
         in
         image_table := !image_table @ images;
         replace gallery container;
         unwrap container);
    main_content
    $$ ".wp-block-image"
    |> iter (fun image_container ->
         let image_node = image_container $ {|> figure > img[class^="wp-image-"]|} in
         let ((new_node, _, _) as image) = extract_image_from_node image_node in
         image_table := !image_table @ [ image ];
         replace image_node new_node);
    !image_table
  in
  main_content
  $$ {|sup > a[href^="#"]|}
  |> iter (fun e ->
       set_attribute "epub:type" "noteref" e;
       (* their footnotes seem to be broken sometimes, so let's fix them *)
       let footnote_name =
         R.attribute "href" e
         |> String.chop_suffix_exn ~suffix:"sym"
         |> String.chop_prefix_exn ~prefix:"#"
       in
       let footnote_answer_node =
         main_content $ Printf.sprintf {|[href=#%sanc]|} footnote_name
       in
       let footnote_text = footnote_answer_node |> R.next_sibling |> R.leaf_text in
       let new_footnote =
         create_element
           "aside"
           ~attributes:[ "id", footnote_name ^ "sym"; "epub:type", "footnote" ]
           ~inner_text:footnote_text
       in
       replace (footnote_answer_node |> R.parent) new_footnote);
  let normalized_name = String.(chapter_name |> lowercase |> tr ~target:' ' ~replacement:'_') in
  let%lwt images =
    image_table
    |> Lwt_list.map_p (fun (node, number, uri) ->
         let%lwt mime, image_bytes = uri |> get_body in
         let path_from_uri = uri |> Uri.path in
         let mime =
           match mime with
           | Some mime -> mime
           | None -> Magic_mime.lookup path_from_uri
         in
         let file_extension =
           let (_, part) = String.lsplit2_exn mime ~on:'/' in
           match String.lsplit2 part ~on:'+' with
           | Some (_, file_extension) -> file_extension
           | None -> part
         in
         let filename = String.concat [
           normalized_name
           ; "_"
           ; string_of_int number
           ; "."
           ; file_extension 
         ] in
         set_attribute "src" ("../Images/" ^ filename) node;
         Lwt.return (filename, image_bytes, mime))
  in
  let xhtml_content =
    let html =
      create_element
        "html"
        ~attributes:
          [ "xmlns", "http://www.w3.org/1999/xhtml"
          ; "xmlns:epub", "http://www.idpf.org/2007/ops"
          ; "lang", "en"
          ; "xml:lang", "en"
          ]
    in
    (* this is kinda hard to read; make some API like Markup.from_tree *)
    let body = create_element "body" in
    append_child body main_content;
    let head = create_element "head" in
    let title = create_element "title" ~inner_text:chapter_name in
    append_child head title;
    append_child html head;
    append_child html body;
    (* DO NOT use Markup.xhtml there, it generates incompatible doctype *)
    xml_header ^ (html |> signals |> Markup.write_xml |> Markup.to_string)
  in
  Lwt.return { name = chapter_name; normalized_name; date; xhtml_content; images }
;;

let dom_element name ?(attrs = []) children =
  (* Technically you need to put namespace URI into those tuples, but i have no idea why,
     so i don't (: *)
  let name = "", name in
  let attrs = List.map ~f:(fun (name, v) -> ("", name), v) attrs in
  `Element (name, attrs, children)
;;

let generate_nav_xhtml full_title chapter_names =
  let open Markup in
  let e = dom_element in
  let toc_entries =
    List.map chapter_names ~f:(fun (name, normalized_name) ->
      let path = String.concat [ "./"; normalized_name; ".xhtml" ] in
      e "li" [ e "a" ~attrs:[ "href", path ] [ `Text name ] ])
  in
  String.( ^ ) xml_header
  (from_tree Fn.id
  @@ e
       "html"
       ~attrs:
         [ "xmlns", "http://www.w3.org/1999/xhtml"
         ; "xmlns:epub", "http://www.idpf.org/2007/ops"
         ; "lang", "en"
         ; "xml:lang", "en"
         ]
       [ e "head" [ e "title" [ `Text full_title ] ]
       ; e "body" [ e "nav" ~attrs:[ "epub:type", "toc" ] [ e "ol" toc_entries ] ]
       ]
  |> write_xml
  |> to_string)
;;

let epub_of_volume ln_title author volume =
  (* TODO: camlzip uses blocking IO from Stdlib, instead support for zip files
     should be added to Decompress *)
  let%lwt cover_art_mimetype, cover_art = get_body volume.cover_art_uri
  and chapters = Lwt_list.map_p download_and_prepare_chapter volume.chapters in

  let cover_art_mimetype =
    Option.value_or_thunk cover_art_mimetype ~default:(fun () ->
      Uri.path volume.cover_art_uri |> Magic_mime.lookup)
  in
  let cover_art_file_extension = file_extension_of_mime cover_art_mimetype in
  let full_title = String.concat [ ln_title; " "; volume.name ] in

  let xml_id_of_name s = String.lowercase s |> String.tr ~target:' ' ~replacement:'-' in

  let open Epub in
  let filename = volume.name ^ ".epub" in
  let metadata =
    let fst_url =
      (* if chapter list is empty, we are doomed anyways, so i'm not going to enable this warning (: *)
      let[@warning "-8"] ((_, fst_uri) :: _) = volume.chapters in
      Uri.to_string fst_uri
    in
    let last_modified =
      chapters
      |> List.map ~f:(fun (c : downloaded_chapter) -> c.date)
      |> List.fold ~init:Time_float.epoch ~f:Time_float.max
      |> epub_datetime_of_time
    in
    { identifier = fst_url
    ; title = full_title
    ; last_modified
    ; date = None
    ; author = if String.(author = "") then None else (Some author) }
  in
  let entries =
    let xhtml_mime = "application/xhtml+xml" in
    let entries_for_chapters =
      List.concat_map chapters ~f:(fun c ->
        { path = String.concat [ "Text/"; c.normalized_name; ".xhtml" ]
        ; content = c.xhtml_content
        ; mimetype = xhtml_mime
        ; id = Some (xml_id_of_name c.name)
        ; properties = [] } ::
        List.map c.images ~f:(fun (filename, content, mimetype) ->
          let filename_without_extension, _ = String.rsplit2_exn ~on:'.' filename in
          { path = "Images/" ^ filename
          ; content
          ; mimetype
          ; id = Some (xml_id_of_name filename_without_extension)
          ; properties = [] }))
    in
    let cover_xhtml =
      String.concat
        [ {xml|<?xml version="1.0" encoding="utf-8"?><!DOCTYPE html><html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops" lang="en" xml:lang="en"><head><title>|xml}
        ; full_title
        ; {xml|</title></head><body><img src="../Images/cover_art.|xml}
        ; cover_art_file_extension
        ; {xml|"/></body></html>|xml}
        ]
    in
    let chapter_names =
      chapters
      |> List.map ~f:(fun {name; normalized_name; _} -> name, normalized_name)
    in
    let nav_xhtml = generate_nav_xhtml full_title chapter_names in
    Epub.(
      { path = "Images/cover_art." ^ cover_art_file_extension
      ; content = cover_art
      ; mimetype = cover_art_mimetype
      ; id = Some "cover"
      ; properties = [ "cover-image" ] } ::
      { path = "Text/nav.xhtml"
      ; content = nav_xhtml
      ; mimetype = xhtml_mime
      ; id = Some "nav"
      ; properties = [ "nav" ] } ::
      { path = "Text/cover.xhtml"
      ; content = cover_xhtml
      ; mimetype = xhtml_mime
      ; id = Some "cover-page"
      ; properties = [] } ::
      entries_for_chapters
    )
  in
  let spine =
    "cover-page" :: 
    "nav" :: 
    (List.map chapters ~f:(fun c -> c.name |> xml_id_of_name))
  in
  create filename metadata entries spine;
  Lwt.return ()
;;

let ccgrab volume_name author url =
  let uri = Uri.of_string url in
  let%lwt _, toc = get_body uri in
  let soup = Soup.parse toc in
  let title = get_title_from_toc soup in
  let volume_names =
    match volume_name with
    | Some volume_name -> [ volume_name ]
    | None -> enumerate_volumes soup
  in
  let volumes = List.map ~f:(parse_volume soup) volume_names in
  Lwt_list.iter_p (epub_of_volume title author) volumes
;;

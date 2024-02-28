(lang dune 3.4)

(name ccgrab)

(generate_opam_files true)

(source
 (github dolphinchips/ccgrab))

(authors "DolphinChips")

(maintainers "DolphinChips")

(license MIT)

(package
 (name ccgrab)
 (synopsis "Download translations from cclawtranslations.home.blog as ePUB files")
 (description "Download translations from cclawtranslations.home.blog as ePUB files")
 (depends
   (cmdliner (= 1.2.0))
   (cohttp-lwt-unix (= 5.3.0))
   (uri (= 4.4.0))
   (lambdasoup (= 1.0.0))
   (core (= v0.16.2))
   (core_unix (= v0.16.0))
   (camlzip (= 1.11))
   (lwt_ppx (= 2.1.0))))

(lang dune 3.4)

(name snazzy_client)

(version 0.1.0-SNAPSHOT)

(generate_opam_files true)

(source
 (github mooreryan/snazzy_client))

(authors "Ryan M. Moore")

(maintainers "Ryan M. Moore")

(license MIT)

(package
 (name snazzy_client)
 (synopsis "Some helpers for Cohttp clients")
 (description "Some helpers for Cohttp clients")
 (depends ocaml dune core async curly uri lambdasoup cohttp-async ppx_jane)
 (tags
  (http client)))

let strf fmt = Printf.kprintf String.trim fmt

let die fmt =
  Printf.ksprintf
    (fun str ->
      Printf.eprintf "%s\n%!" str;
      exit 1)
    fmt

type t = Hvt | Spt | Virtio | Muen | Genode

let bindings =
  [
    ("hvt", Hvt);
    ("spt", Spt);
    ("virtio", Virtio);
    ("muen", Muen);
    ("genode", Genode);
  ]

let to_string t =
  let rev_bindings = List.map (fun (x, y) -> (y, x)) bindings in
  List.assoc t rev_bindings

let of_string s =
  try List.assoc s bindings with Not_found -> die "unknown bindings: %s" s

(* emulate the dune compact form for lists *)
let compact_list indent l =
  let all = Buffer.create 1024 in
  let line = Buffer.create 70 in
  let sep = "\n" ^ String.make indent ' ' in
  let first_line = ref true in
  let flush () =
    Buffer.add_buffer all line;
    Buffer.clear line;
    Buffer.add_string line sep;
    first_line := false
  in
  List.iter
    (fun w ->
      let max = if !first_line then 70 - indent else 75 in
      let wn = String.length w in
      if wn >= 40 || Buffer.length line + wn >= max then flush ();
      Buffer.add_char line ' ';
      Buffer.add_string line w)
    l;
  flush ();
  Buffer.contents all

let list indent l =
  let sep = "\n" ^ String.make indent ' ' in
  let concat sep = sep ^ String.concat sep l in
  let short = concat " " in
  if String.length short <= 70 then short else concat sep

module Config : sig
  val make : int -> t -> string
end = struct
  let config main t =
    let n = if t = main then "1" else "" in
    let t = String.uppercase_ascii (to_string t) in
    "CONFIG_" ^ t ^ "=" ^ n

  let make indent t =
    let config (_, x) = config t x in
    let configs = List.map config bindings in
    let sep = "\n" ^ String.make (max 0 (indent - 1)) ' ' in
    let configs =
      List.mapi (fun i c -> if i mod 4 = 3 then c ^ sep else c) configs
    in
    String.concat " " configs
end

module Files : sig
  val dune : t -> string
end = struct
  type file = { src : string; dst : string }

  let file ?dst fmt =
    Printf.ksprintf
      (fun s ->
        let dst = match dst with None -> Filename.basename s | Some s -> s in
        { src = s; dst })
      fmt

  let lib t =
    let bindings f = file "bindings/%s/%s" (to_string t) f in
    match t with
    | Genode -> [ bindings "solo5.lib.so"; bindings "genode_dyn.ld" ]
    | _ ->
        let solo5 ext = bindings (strf "solo5_%s.%s" (to_string t) ext) in
        [ solo5 "o"; solo5 "lds" ]

  let bin t =
    let tender s = file "tenders/%s/solo5-%s%s" (to_string t) (to_string t) s in
    let script s =
      let dst = strf "solo5-virtio-%s" s in
      file ~dst "scripts/virtio-%s/%s.sh" s dst
    in
    match t with
    | Hvt ->
        [ tender ""; tender "-configure"; tender "-debug" ]
        (* TODO: -debug is not available on OpenBSD *)
    | Spt -> [ tender "" ]
    | Virtio -> [ script "mkimage"; script "run" ]
    | Muen | Genode -> []

  let main t =
    let s = to_string t in
    strf
      {|
(data_only_dirs solo5)

(library
 (public_name solo5-%s)
 (name %s)
 (libraries solo5)
 (modules))

(rule
 (targets cflags)
 (deps solo5/cflags.pc %%{lib:solo5-%s:dune-package})
 (action
  (with-stdout-to
   %%{targets}
   (progn
    (bash "echo \"($(cat solo5/cflags.pc) -isystem \"")
    (bash
     ; FIXME: do not use realpath and dirname
     "echo \"$(realpath $(dirname %%{lib:solo5:dune-package}))/crt\"")
    (bash
     ; FIXME: do not use realpath and dirname
     "echo \" -I$(realpath $(dirname %%{lib:solo5:dune-package}))\"")
    (bash
     ; FIXME: do not use realpath and dirname
     "echo \" -I$(realpath $(dirname %%{lib:solo5-%s:dune-package})))\"")))))

(rule
 (targets ldflags)
 (deps %%{lib:solo5-%s:dune-package})
 (action
  (with-stdout-to
   %%{targets}
   (progn
    (echo "%%{read:solo5/ldflags.pc}")
    (bash
     ; FIXME: do not use realpath and dirname
     "echo \"-L$(realpath $(dirname %%{lib:solo5-%s:dune-package}))\"")
    (echo " -lasmrun -lnolibc -lopenlibm")))))

(install
 (files cflags ldflags)
 (section lib)
 (package solo5-%s))
|}
      s s s s s s s

  let solo5 t =
    let name = to_string t in
    let public_name = "solo5-" ^ to_string t in
    let sources l = compact_list 3 l in
    let install indent l =
      compact_list indent
        (List.map
           (fun l ->
             let src = Filename.basename l.src in
             if src = l.dst then l.dst else strf "(%s as %s)" src l.dst)
           l)
    in
    let bins = bin t in
    let libs = lib t in
    let all_files = bins @ libs in
    let copies =
      list 4 (List.map (fun l -> strf "(bash \"cp -R %s .\")" l.src) all_files)
    in
    let targets = List.map (fun l -> Filename.basename l.src) all_files in
    let config = Config.make 6 t in
    let bin =
      match bins with
      | [] -> ""
      | _ ->
          strf {|
 (install
  (files%s)
  (section bin)
  (package %s))

  |}
            (install 2 bins) public_name
    in
    strf
      {|
;; %s
 (rule
  (targets cflags.pc)
  (package %s)
  (deps
   (source_tree .)
   ../../../files/cflags.pc.in)
  (action
   (progn
    (bash "cp ../../../files/cflags.pc.in .")
    (bash "./configure.sh")
    (run %%{make} %s %%{targets}))))
 (rule
  (targets ldflags.pc)
  (package %s)
  (deps
   (source_tree .)
   ../../../files/ldflags.pc.in)
  (action
   (progn
    (bash "cp ../../../files/ldflags.pc.in .")
    (bash "./configure.sh")
    (run %%{make} %s %%{targets}))))
 (rule
  (targets%s)
  (package %s)
  (deps
   (source_tree .))
  (action
   (progn
    (bash "./configure.sh")
    (run %%{make} %s)%s)))
 (install
  (files%s)
  (section lib)
  (package %s))
 %s
|}
      (String.uppercase_ascii name)
      public_name config public_name config (sources targets) public_name config
      copies (install 3 libs) public_name bin

  let nolibc t =
    let s = to_string t in
    strf
      {|
 (foreign_library
  (archive_name nolibc)
  (language c)
  (names ctype dtoa memchr memcmp memcpy memmove memset strcmp strlen strtol
    strchr strchrnul strncpy stpncpy strstr stubs vfprintf vsnprintf snprintf
    fprintf printf sysdeps_solo5)
  (flags
   -std=c99
   -Wall
   -Wno-parentheses
   -Werror
   -O2
   (:include ../cflags)
   -nostdlib
   -isystem
   ./include
   -isystem
   ../openlibm/src
   -isystem
   ../openlibm/include)
  (extra_deps
   (source_tree .)
   (source_tree ../openlibm)
   %%{lib:solo5:crt}
   %%{lib:solo5:solo5.h}))
 (install
  (section lib)
  (package solo5-%s)
  (files libnolibc.a))
|}
      s

  let openlibm t =
    let s = to_string t in
    strf
      {|
 (env
  (_
   (c_flags
    (:include ../cflags)
    -nostdlib
    -isystem
    ../nolibc/include)))
 (rule
  (deps
   (source_tree .)
   (source_tree ../nolibc)
   %%{lib:solo5:crt}
   %%{lib:solo5:solo5.h})
  (targets libopenlibm.a)
  (package solo5-%s)
  (action
   (run ../../../scripts/build-openlibm.sh %%{cc})))
 (install
  (section lib)
  (package solo5-%s)
  (files libopenlibm.a include/openlibm.h include/openlibm_complex.h
    include/openlibm_fenv.h include/openlibm_fenv_amd64.h
    include/openlibm_fenv_arm.h include/openlibm_fenv_i387.h
    include/openlibm_fenv_powerpc.h include/openlibm_math.h
    src/aarch64_fpmath.h src/fpmath.h src/math_private.h src/amd64_fpmath.h
    src/i386_fpmath.h src/math_private_openbsd.h src/bsd_cdefs.h src/k_log.h
    src/powerpc_fpmath.h src/cdefs-compat.h src/k_logf.h src/types-compat.h))
|}
      s s

  let ocaml t =
    let s = to_string t in
    strf
      {|
 ; FIXME: See ocaml/dune#3387 for better rules
 (subdir
  runtime/caml
  (copy_files ../../../../../vendor/ocaml/%%{ocaml_version}/runtime/caml/**))
 (subdir
  runtime
  (copy_files ../../../../vendor/ocaml/%%{ocaml_version}/runtime/**))
 (subdir
  tools
  (copy_files ../../../../vendor/ocaml/%%{ocaml_version}/tools/**))
 (subdir
  build-aux
  (copy_files ../../../../vendor/ocaml/%%{ocaml_version}/build-aux/**))
 (copy_files ../../../vendor/ocaml/%%{ocaml_version}/**)
 (rule
  (targets cflags)
  (deps
   (env_var PWD))
  (action
   (with-stdout-to
    %%{targets}
    ; FIXME: do not use realpath
    (bash
      "echo \"(-nostdlib -isystem $(realpath ../nolibc/include) -I$(realpath ../openlibm/include) -I$(realpath ../openlibm/src))\""))))
 (env
  (_
   (c_flags
    (:include ../cflags)
    (:include cflags))))
 (rule
  (deps
   (env_var OCAML_TARGET)
   ; FIXME: source_tree deps doesn't work with copy_files
   (glob_files configure*)
   (glob_files {Makefile,Makefile.*.in})
   (glob_files runtime/**.[!a])
   (glob_files runtime/caml/**)
   (glob_files tools/**)
   (glob_files build-aux/**)
   %%{lib:solo5:crt}
   %%{lib:solo5:solo5.h}
   (source_tree ../nolibc)
   (source_tree ../openlibm))
  ; s.h and m.h are listed here as dune doesn't like targets in subdir ocaml/dune#3374
  (targets Makefile.config Makefile.common s.h m.h version.h domain.h
    domain_state.h domain_state.tbl)
  (action
   (run ../../../scripts/configure-ocaml.sh %%{ocaml-config:target} %%{cc})))
 (subdir
  runtime
  (rule
   (deps
    ; FIXME: source_tree deps doesn't work with copy_files
    Makefile
    .depend
    (glob_files caml/**)
    ; normal deps
    (source_tree ../../nolibc)
    (source_tree ../../openlibm)
    %%{lib:solo5:crt}
    %%{lib:solo5:solo5.h}
    ../VERSION
    ../Makefile.config
    ../Makefile.common
    ../s.h
    ../m.h
    ../version.h)
   (targets libasmrun.a)
   (package solo5-%s)
   (action
    (progn
     (run cp ../s.h ../m.h ../version.h caml)
     (run %%{make} libasmrun.a))))
  (install
   (section lib)
   (package solo5-%s)
   (files
    libasmrun.a
    (caml/alloc.h as caml/alloc.h)
    (caml/callback.h as caml/callback.h)
    (caml/config.h as caml/config.h)
    (caml/custom.h as caml/custom.h)
    (caml/fail.h as caml/fail.h)
    (caml/hash.h as caml/hash.h)
    (caml/intext.h as caml/intext.h)
    (caml/memory.h as caml/memory.h)
    (caml/misc.h as caml/misc.h)
    (caml/mlvalues.h as caml/mlvalues.h)
    (caml/printexc.h as caml/printexc.h)
    (caml/signals.h as caml/signals.h)
    (caml/compatibility.h as caml/compatibility.h)
    (../m.h as caml/m.h)
    (../s.h as caml/s.h)
    (../domain.h as caml/domain.h)
    (../domain_state.h as caml/domain_state.h)
    (../domain_state.tbl as caml/domain_state.tbl))))
|}
      s s

  let dune t =
    strf
      {|
%s

(subdir
 solo5
 %s)

(subdir
 nolibc
 %s)

(subdir
 openlibm
 %s)

(subdir
 ocaml
 %s)
|}
      (main t) (solo5 t) (nolibc t) (openlibm t) (ocaml t)
end

let dune t = Printf.printf "%s\n" (Files.dune t)

let dune_inc () =
  let dunes =
    List.map
      (fun (s, _) ->
        strf
          {|
(rule
 (with-stdout-to
  dune.%s.gen
  (run ./build.exe %s)))

(rule
 (alias runtest)
 (action
  (diff dune.%s dune.%s.gen)))
|}
          s s s s)
      bindings
  in
  Printf.printf "%s\n" (String.concat "\n\n" dunes)

let usage () =
  let bindings = String.concat "|" (List.map fst bindings) in
  die "usage: ./build.exe [dune]\n    ./build.exe [%s] <install-dir>" bindings

let () =
  if Array.length Sys.argv <> 2 then usage ()
  else match Sys.argv.(1) with "dune" -> dune_inc () | s -> dune (of_string s)

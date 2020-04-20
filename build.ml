let ( / ) = Filename.concat

let strf fmt = Printf.kprintf String.trim fmt

let die fmt =
  Printf.ksprintf
    (fun str ->
      Printf.eprintf "%s\n%!" str;
      exit 1)
    fmt

type t = Hvt | Spt | Virtio | Muen | Genode | Solo5

let bindings =
  [
    ("solo5", Solo5);
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

module Config : sig
  val make : t -> string
end = struct
  let config main t =
    let n = if t = main then "1" else "" in
    let t = String.uppercase_ascii (to_string t) in
    "CONFIG_" ^ t ^ "=" ^ n

  let make t =
    let config (_, x) = config t x in
    let configs =
      List.map config (List.filter (fun (_, t) -> t <> Solo5) bindings)
    in
    String.concat " " configs
end

let dir t = "src-" ^ to_string t

module Files : sig
  val generated : t -> string list

  val dune : t -> string
end = struct
  type file = { src : string; dst : string }

  let file t ?dst ?(install = true) fmt =
    Printf.ksprintf
      (fun s ->
        let dst = match dst with None -> Filename.basename s | Some s -> s in
        { src = dir t / s; dst })
      fmt

  let lib t =
    let bindings f = file t "bindings/%s/%s" (to_string t) f in
    let flags =
      [
        (* dune complains if the same file is installed by multiple stanzas *)
        file t "%s-cflags" (to_string t) ~dst:"cflags";
        file t "%s-ldflags" (to_string t) ~dst:"ldflags";
      ]
    in
    match t with
    | Solo5 ->
        let file s = file t "include/%s" s ~dst:s in
        [
          file "solo5/elf_abi.h";
          file "solo5/hvt_abi.h";
          file "solo5/mft_abi.h";
          file "solo5/solo5.h";
          file "solo5/solo5_version.h";
          file "solo5/spt_abi.h";
          (* see https://gcc.gnu.org/onlinedocs/gcc/Standards.html#Standards *)
          file "crt/float.h";
          (* FIXME: missing? file "crt/limits.h"; *)
          file "crt/stdarg.h";
          file "crt/stddef.h";
          file "crt/iso646.h";
          file "crt/stdbool.h";
          file "crt/stdint.h";
          file "crt/stdint-gcc.h";
          file "crt/stdalign.h";
          file "crt/stdnoreturn.h";
        ]
    | Genode -> bindings "solo5.lib.so" :: bindings "genode_dyn.ld" :: flags
    | _ ->
        let solo5 ext = bindings (strf "solo5_%s.%s" (to_string t) ext) in
        solo5 "o" :: solo5 "lds" :: flags

  let bin t =
    let tender s =
      file t "tenders/%s/solo5-%s%s" (to_string t) (to_string t) s
    in
    let script s =
      let dst = strf "solo5-virtio-%s" s in
      file ~dst t "scripts/virtio-%s/%s.sh" s dst
    in
    match t with
    | Hvt ->
        [ tender ""; tender "-configure"; tender "-debug" ]
        (* TODO: -debug is not available on OpenBSD *)
    | Spt -> [ tender "" ]
    | Virtio -> [ script "mkimage"; script "run" ]
    | Solo5 -> [ file t "elftool/solo5-elftool" ]
    | Muen | Genode -> []

  let generated t = List.map (fun t -> t.src) (lib t @ bin t)

  let dune t =
    let name = to_string t in
    let public_name =
      match t with Solo5 -> "solo5" | t -> "solo5-" ^ to_string t
    in
    let list l =
      let concat sep = sep ^ String.concat sep l in
      let short = concat " " in
      if String.length short <= 70 then short else concat "\n   "
    in
    let sources l = list (List.map (fun l -> Filename.basename l.src) l) in
    let install l =
      list
        (List.map
           (fun l ->
             let src = Filename.basename l.src in
             if src = l.dst then l.dst else strf "(%s as %s)" src l.dst)
           l)
    in
    let deps =
      let all = [ "build.ml"; "(source_tree src)" ] in
      match t with
      | Solo5 -> all
      | Genode -> "genode-cflags.pc.in" :: "genode-ldflags.pc.in" :: all
      | _ -> "cflags.pc.in" :: "ldflags.pc.in" :: all
    in
    let install_dir =
      match t with
      | Solo5 -> ""
      | _ -> " $(realpath $(dirname %{lib:solo5:META}))"
    in
    let bins = bin t in
    let libs = lib t in
    let targets = bins @ libs in
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
            (install bins) public_name
    in
    strf
      {|
;; %s

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name %s)
  (name %s)
  (modules))

(rule
 (targets%s)
 (deps%s)
 (package %s)
 (action (bash "ocaml build.ml %s%s")))

(install
  (files%s)
  (section lib)
  (package %s))

%s
|}
      (String.uppercase_ascii name)
      public_name name (sources targets) (list deps) public_name name
      install_dir (install libs) public_name bin
end

let exec fmt =
  Printf.kprintf
    (fun str ->
      Printf.printf "=> %s\n%!" str;
      match Sys.command str with 0 -> () | i -> exit i)
    fmt

let chdir t =
  Printf.printf "CHDIR %s\n%!" (dir t);
  Sys.chdir (dir t)

let read_line file =
  let ic = open_in file in
  let line = String.trim (input_line ic) in
  close_in ic;
  line

let write_line file line =
  let oc = open_out file in
  output_string oc line;
  close_out oc

let gen_flags t install_dir =
  match t with
  | Solo5 -> ()
  | t ->
      let src_cflags =
        strf "../%s.pc.in"
          (match t with Genode -> "genode-cflags" | _ -> "cflags")
      in
      let src_ldflags =
        strf "../%s.pc.in"
          (match t with Genode -> "genode-ldflags" | _ -> "ldflags")
      in
      let cflags = strf "%s-cflags.pc" (dir t) in
      let ldflags = strf "%s-ldflags.pc" (dir t) in
      let ocaml_cflags = strf "%s-cflags" (to_string t) in
      let ocaml_ldflags = strf "%s-ldflags" (to_string t) in

      (* Copy the .pc.in files in and call [make] to substitue CFLAGS and
         LDFLAGS variables. *)
      exec "cp %s %s.in" src_cflags cflags;
      exec "cp %s %s.in" src_ldflags ldflags;
      exec "make %s %s" cflags ldflags;

      (* Read CFLAGS and LDFLAGS ; just add [( ... )] so that these can be
         included by dune actions with [(:include %{lib:solo5-*:*flags}] .*)
      Printf.printf "=> Generating %S\n%!" ocaml_cflags;
      let cflags = read_line cflags in
      let cflags =
        strf "%s -isystem %s/crt -I%s/solo5" cflags install_dir install_dir
      in
      write_line ocaml_cflags cflags;

      Printf.printf "=> Generating %S\n%!" ocaml_ldflags;
      let ldflags = read_line ldflags in
      write_line ocaml_ldflags ldflags

let run t install_dir =
  exec "cp -R src %s" (dir t);
  exec "chmod +w %s" (dir t);
  chdir t;
  exec "./configure.sh";
  exec "make %s\n%!" (Config.make t);
  gen_flags t install_dir;
  Sys.chdir "..";
  exec "chmod -R +w .";
  List.iter
    (fun src ->
      let dst = Filename.basename src in
      if Sys.is_directory src then exec "cp -R %s %s" src dst
      else exec "cp %s %s" src dst)
    (Files.generated t)

let dune () =
  let dunes = List.map (fun (_, t) -> Files.dune t) bindings in
  Printf.printf "%s\n" (String.concat "\n\n" dunes)

let usage () =
  let bindings = String.concat "|" (List.map fst bindings) in
  die "usage: build.ml [dune]\n    build.ml [%s] <install-dir>" bindings

let () =
  match Array.length Sys.argv with
  | 3 -> run (of_string Sys.argv.(1)) Sys.argv.(2)
  | 2 -> (
      match Sys.argv.(1) with
      | "dune" -> dune ()
      | "solo5" -> run Solo5 ""
      | _ -> usage () )
  | _ -> usage ()

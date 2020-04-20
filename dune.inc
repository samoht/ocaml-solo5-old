;; SOLO5

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name solo5)
  (name solo5)
  (modules))

(rule
 (targets
   solo5-elftool
   elf_abi.h
   hvt_abi.h
   mft_abi.h
   solo5.h
   solo5_version.h
   spt_abi.h
   float.h
   stdarg.h
   stddef.h
   iso646.h
   stdbool.h
   stdint.h
   stdint-gcc.h
   stdalign.h
   stdnoreturn.h)
 (deps build.ml (source_tree src))
 (package solo5)
 (action (bash "ocaml build.ml solo5")))

(install
  (files
   (elf_abi.h as solo5/elf_abi.h)
   (hvt_abi.h as solo5/hvt_abi.h)
   (mft_abi.h as solo5/mft_abi.h)
   (solo5.h as solo5/solo5.h)
   (solo5_version.h as solo5/solo5_version.h)
   (spt_abi.h as solo5/spt_abi.h)
   (float.h as crt/float.h)
   (stdarg.h as crt/stdarg.h)
   (stddef.h as crt/stddef.h)
   (iso646.h as crt/iso646.h)
   (stdbool.h as crt/stdbool.h)
   (stdint.h as crt/stdint.h)
   (stdint-gcc.h as crt/stdint-gcc.h)
   (stdalign.h as crt/stdalign.h)
   (stdnoreturn.h as crt/stdnoreturn.h))
  (section lib)
  (package solo5))

(install
  (files solo5-elftool)
  (section bin)
  (package solo5))

;; HVT

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name solo5-hvt)
  (name hvt)
  (modules))

(rule
 (targets
   solo5-hvt
   solo5-hvt-configure
   solo5-hvt-debug
   solo5_hvt.o
   solo5_hvt.lds
   hvt-cflags
   hvt-ldflags)
 (deps cflags.pc.in ldflags.pc.in build.ml (source_tree src))
 (package solo5-hvt)
 (action (bash "ocaml build.ml hvt $(realpath $(dirname %{lib:solo5:META}))")))

(install
  (files
   solo5_hvt.o
   solo5_hvt.lds
   (hvt-cflags as cflags)
   (hvt-ldflags as ldflags))
  (section lib)
  (package solo5-hvt))

(install
  (files solo5-hvt solo5-hvt-configure solo5-hvt-debug)
  (section bin)
  (package solo5-hvt))

;; SPT

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name solo5-spt)
  (name spt)
  (modules))

(rule
 (targets solo5-spt solo5_spt.o solo5_spt.lds spt-cflags spt-ldflags)
 (deps cflags.pc.in ldflags.pc.in build.ml (source_tree src))
 (package solo5-spt)
 (action (bash "ocaml build.ml spt $(realpath $(dirname %{lib:solo5:META}))")))

(install
  (files
   solo5_spt.o
   solo5_spt.lds
   (spt-cflags as cflags)
   (spt-ldflags as ldflags))
  (section lib)
  (package solo5-spt))

(install
  (files solo5-spt)
  (section bin)
  (package solo5-spt))

;; VIRTIO

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name solo5-virtio)
  (name virtio)
  (modules))

(rule
 (targets
   solo5-virtio-mkimage.sh
   solo5-virtio-run.sh
   solo5_virtio.o
   solo5_virtio.lds
   virtio-cflags
   virtio-ldflags)
 (deps cflags.pc.in ldflags.pc.in build.ml (source_tree src))
 (package solo5-virtio)
 (action (bash "ocaml build.ml virtio $(realpath $(dirname %{lib:solo5:META}))")))

(install
  (files
   solo5_virtio.o
   solo5_virtio.lds
   (virtio-cflags as cflags)
   (virtio-ldflags as ldflags))
  (section lib)
  (package solo5-virtio))

(install
  (files
   (solo5-virtio-mkimage.sh as solo5-virtio-mkimage)
   (solo5-virtio-run.sh as solo5-virtio-run))
  (section bin)
  (package solo5-virtio))

;; MUEN

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name solo5-muen)
  (name muen)
  (modules))

(rule
 (targets solo5_muen.o solo5_muen.lds muen-cflags muen-ldflags)
 (deps cflags.pc.in ldflags.pc.in build.ml (source_tree src))
 (package solo5-muen)
 (action (bash "ocaml build.ml muen $(realpath $(dirname %{lib:solo5:META}))")))

(install
  (files
   solo5_muen.o
   solo5_muen.lds
   (muen-cflags as cflags)
   (muen-ldflags as ldflags))
  (section lib)
  (package solo5-muen))

;; GENODE

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name solo5-genode)
  (name genode)
  (modules))

(rule
 (targets solo5.lib.so genode_dyn.ld genode-cflags genode-ldflags)
 (deps genode-cflags.pc.in genode-ldflags.pc.in build.ml (source_tree src))
 (package solo5-genode)
 (action (bash "ocaml build.ml genode $(realpath $(dirname %{lib:solo5:META}))")))

(install
  (files
   solo5.lib.so
   genode_dyn.ld
   (genode-cflags as cflags)
   (genode-ldflags as ldflags))
  (section lib)
  (package solo5-genode))

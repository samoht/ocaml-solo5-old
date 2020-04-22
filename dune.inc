;; SOLO5

; a dummy OCaml library is needed because of ocaml/dune#3378
(library
  (public_name solo5)
  (name solo5)
  (modules))

(rule
 (targets
   solo5-elftool
   solo5-cflags
   elf_abi.h
   hvt_abi.h
   mft_abi.h
   solo5.h
   solo5_version.h
   spt_abi.h
   crt)
 (deps build.exe (source_tree src) %{lib:solo5:dune-package})
 (package solo5)
 (action (progn
   (bash "cp -R src src-solo5")
   (bash
     "./build.exe solo5 \
       $(realpath $(dirname %{lib:solo5:dune-package})) \
       $(realpath $(dirname %{lib:solo5:dune-package}))"))))

(install
  (files
   (solo5-cflags as cflags)
   elf_abi.h
   hvt_abi.h
   mft_abi.h
   solo5.h
   solo5_version.h
   spt_abi.h
   crt)
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
  (libraries solo5)
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
 (deps
   cflags.pc.in
   ldflags.pc.in
   build.exe
   (source_tree src)
   %{lib:solo5-hvt:dune-package})
 (package solo5-hvt)
 (action (progn
   (bash "cp -R src src-hvt")
   (bash
     "./build.exe hvt \
       $(realpath $(dirname %{lib:solo5:dune-package})) \
       $(realpath $(dirname %{lib:solo5-hvt:dune-package}))"))))

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
  (libraries solo5)
  (modules))

(rule
 (targets solo5-spt solo5_spt.o solo5_spt.lds spt-cflags spt-ldflags)
 (deps
   cflags.pc.in
   ldflags.pc.in
   build.exe
   (source_tree src)
   %{lib:solo5-spt:dune-package})
 (package solo5-spt)
 (action (progn
   (bash "cp -R src src-spt")
   (bash
     "./build.exe spt \
       $(realpath $(dirname %{lib:solo5:dune-package})) \
       $(realpath $(dirname %{lib:solo5-spt:dune-package}))"))))

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
  (libraries solo5)
  (modules))

(rule
 (targets
   solo5-virtio-mkimage.sh
   solo5-virtio-run.sh
   solo5_virtio.o
   solo5_virtio.lds
   virtio-cflags
   virtio-ldflags)
 (deps
   cflags.pc.in
   ldflags.pc.in
   build.exe
   (source_tree src)
   %{lib:solo5-virtio:dune-package})
 (package solo5-virtio)
 (action (progn
   (bash "cp -R src src-virtio")
   (bash
     "./build.exe virtio \
       $(realpath $(dirname %{lib:solo5:dune-package})) \
       $(realpath $(dirname %{lib:solo5-virtio:dune-package}))"))))

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
  (libraries solo5)
  (modules))

(rule
 (targets solo5_muen.o solo5_muen.lds muen-cflags muen-ldflags)
 (deps
   cflags.pc.in
   ldflags.pc.in
   build.exe
   (source_tree src)
   %{lib:solo5-muen:dune-package})
 (package solo5-muen)
 (action (progn
   (bash "cp -R src src-muen")
   (bash
     "./build.exe muen \
       $(realpath $(dirname %{lib:solo5:dune-package})) \
       $(realpath $(dirname %{lib:solo5-muen:dune-package}))"))))

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
  (libraries solo5)
  (modules))

(rule
 (targets solo5.lib.so genode_dyn.ld genode-cflags genode-ldflags)
 (deps
   genode-cflags.pc.in
   genode-ldflags.pc.in
   build.exe
   (source_tree src)
   %{lib:solo5-genode:dune-package})
 (package solo5-genode)
 (action (progn
   (bash "cp -R src src-genode")
   (bash
     "./build.exe genode \
       $(realpath $(dirname %{lib:solo5:dune-package})) \
       $(realpath $(dirname %{lib:solo5-genode:dune-package}))"))))

(install
  (files
   solo5.lib.so
   genode_dyn.ld
   (genode-cflags as cflags)
   (genode-ldflags as ldflags))
  (section lib)
  (package solo5-genode))

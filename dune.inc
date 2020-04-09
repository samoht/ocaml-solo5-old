;; SOLO5

(rule
 (targets solo5-elftool solo5 crt)
 (deps
   cflags.pc.in
   ldflags.pc.in
   genode-cflags.pc.in
   genode-ldflags.pc.in
   build.ml
   (source_tree src))
 (package solo5)
 (action (run ocaml build.ml solo5)))

(install
  (files solo5 crt)
  (section lib)
  (package solo5))

(install
  (files solo5-elftool)
  (section bin)
  (package solo5))

;; HVT

(rule
 (targets
   solo5-hvt
   solo5-hvt-configure
   solo5-hvt-debug
   solo5_hvt.o
   solo5_hvt.lds
   hvt-cflags
   hvt-ldflags)
 (deps build.ml (source_tree src))
 (package solo5-hvt)
 (action (run ocaml build.ml hvt)))

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

(rule
 (targets solo5-spt solo5_spt.o solo5_spt.lds spt-cflags spt-ldflags)
 (deps build.ml (source_tree src))
 (package solo5-spt)
 (action (run ocaml build.ml spt)))

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

(rule
 (targets
   solo5-virtio-mkimage.sh
   solo5-virtio-run.sh
   solo5_virtio.o
   solo5_virtio.lds
   virtio-cflags
   virtio-ldflags)
 (deps build.ml (source_tree src))
 (package solo5-virtio)
 (action (run ocaml build.ml virtio)))

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

(rule
 (targets solo5_muen.o solo5_muen.lds muen-cflags muen-ldflags)
 (deps build.ml (source_tree src))
 (package solo5-muen)
 (action (run ocaml build.ml muen)))

(install
  (files
   solo5_muen.o
   solo5_muen.lds
   (muen-cflags as cflags)
   (muen-ldflags as ldflags))
  (section lib)
  (package solo5-muen))

;; GENODE

(rule
 (targets solo5.lib.so genode_dyn.ld genode-cflags genode-ldflags)
 (deps build.ml (source_tree src))
 (package solo5-genode)
 (action (run ocaml build.ml genode)))

(install
  (files
   solo5.lib.so
   genode_dyn.ld
   (genode-cflags as cflags)
   (genode-ldflags as ldflags))
  (section lib)
  (package solo5-genode))
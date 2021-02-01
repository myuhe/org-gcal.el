;;; org-generic-id-test.el --- Tests for org-generic-id.el -*- lexical-binding: t -*-

(require 'org-generic-id)
(require 'cl-lib)
(require 'el-mock)

(ert-deftest org-generic-id--round-trip ()
  "Verify that ‘org-generic-id-locations-save’ and
‘org-generic-id-locations-load' round-trip."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-generic-id-locations-alist
          '(("ID"
             ("/tmp/bar" "id1" "id2" "id3")
             ("/tmp/foo" "fid"))
            ("SN"
             ("/tmp/bar" "sn1" "sna"))))
         (org-generic-id-locations
          (org-generic-id--locations-alist-to-hash
           org-generic-id-locations-alist)))
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    (should (equal (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                   org-generic-id-locations-alist))))

(ert-deftest org-generic-id--find ()
  "Verify ‘org-generic-id-find’.
‘org-generic-id-locations-load' round-trip."
  (let* ((org-generic-id-locations-file
          (make-temp-file "org-generic-id-locations."))
         (org-file
          ;; When I rescan ID locations, the symlink "/var" is resolved to
          ;; "/private/var" on macOS. The simplest way to fix this is just to
          ;; resolve the symlink manually at the start.
          (file-truename (make-temp-file "my.org.")))
         (org-agenda-files (list org-file))
         (org-generic-id-extra-files nil)
         (org-generic-id-locations-alist
          `(("ID"
             (,org-file "id1"))
            ("SN"
             (,org-file "sn1"))))
         (org-generic-id-locations
          (org-generic-id--locations-alist-to-hash
           org-generic-id-locations-alist)))
    ;; Reset various variables by saving and reloading.
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    (with-temp-file org-file
      (org-mode)
      (insert "\
* id1
:PROPERTIES:
:ID: id1
:SN: sn1
:END:

* id2
:PROPERTIES:
:ID: id2
:SN: sn2
:END:"))
    (let (m)
      (setq m (org-generic-id-find-id-in-file
               "ID" "id1" org-file))
      (should (equal m
                     `(,org-file . 1)))
      (setq m (org-generic-id-find "ID" "id1"))
      (should (equal m
                     `(,org-file . 1)))
      (setq m (org-generic-id-find "ID" "id2" nil t))
      (should (equal m nil))
      (setq m (org-generic-id-find "ID" "id2"))
      (should (equal m
                     `(,org-file . 45)))

      (setq m (org-generic-id-find-id-in-file
               "SN" "sn1" org-file))
      (should (equal m
                     `(,org-file . 1)))
      (setq m (org-generic-id-find "SN" "sn1"))
      (should (equal m
                     `(,org-file . 1)))
      (setq m (org-generic-id-find "SN" "sn2" nil t))
      (should (equal m nil))
      (setq m (org-generic-id-find "SN" "sn2"))
      (should (equal m
                     `(,org-file . 45))))))

;;; org-generic-id.el --- Global identifiers for Org entries -*- lexical-binding: t; -*-
;;
;; Author: Robert Irelan <rirelan at gmail dot com>
;; Homepage: https://orgmode.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file generalizes ‘org-id' to allow using any property, not just :ID:,
;; to locate Org-mode entries. This library only handles storing IDs in entries,
;; locating entries with a given ID, and storing a map of IDs to files
;; containing such IDs. Generation of IDs must be handled by the user of the
;; library - unlike ‘org-id', no functions are provided to automatically
;; generate IDs.

;;; Code:

(require 'org)

(declare-function message-make-fqdn "message" ())
(declare-function org-goto-location "org-goto" (&optional _buf help))

;;; Customization

(defgroup org-generic-id nil
  "Options concerning global entry identifiers in Org mode."
  :tag "Org ID"
  :group 'org)

(defcustom org-generic-id-locations-file (convert-standard-filename
                                          (concat user-emacs-directory ".org-generic-id-locations"))
  "The file for remembering in which file an ID was defined."
  :group 'org-generic-id
  :type 'file)

(defcustom org-generic-id-locations-file-relative nil
  "Determines if org-generic-id-locations should be stored as relative links.
Non-nil means that links to locations are stored as links
relative to the location of where `org-generic-id-locations-file' is
stored.

Nil means to store absolute paths to files.

This customization is useful when folders are shared across
systems but mounted at different roots.  Relative path to
`org-generic-id-locations-file' still has to be maintained across
systems."
  :group 'org-generic-id
  :type 'boolean)

(defvar org-generic-id-locations nil
  "List of files with IDs in those files.")

(defvar org-generic-id-files nil
  "List of files that contain IDs.")

(defcustom org-generic-id-extra-files 'org-agenda-text-search-extra-files
  "Files to be searched for IDs, besides the agenda files.
When Org reparses files to remake the list of files and IDs it is tracking,
it will normally scan the agenda files, the archives related to agenda files,
any files that are listed as ID containing in the current register, and
any Org file currently visited by Emacs.
You can list additional files here."
  :group 'org-generic-id
  :type
  '(choice
    (symbol :tag "Variable")
    (repeat :tag "List of files"
            (file))))

(defcustom org-generic-id-search-archives t
  "Non-nil means search also the archive files of agenda files for entries.
This is a possibility to reduce overhead, but it means that entries moved
to the archives can no longer be found by ID."
  :group 'org-generic-id
  :type 'boolean)

;;; The API functions

;;;###autoload
(defun org-generic-id-get (&optional id-prop pom)
  "Get the ID-PROP property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
In any case, the ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil id-prop)))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (t nil)))))

;;;###autoload
(defun org-generic-id-find (id-prop id &optional markerp cached no-fallback)
  "Return the location of the entry with property ID-PROP, value ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker.

Normally, if an entry with ID is not found, this function will run
‘org-generic-id-update-id-locations' in order to pick up any updates to the
files, and then search again, before concluding an ID can’t be found. If
CACHED is passed, that function will not be run.

Normally the ID will be searched for in the current buffer before updating ID
locations. This behavior can be disabled with NO-FALLBACK."
  (cond
   ((symbolp id) (setq id (symbol-name id)))
   ((numberp id) (setq id (number-to-string id))))
  (let ((file (org-generic-id-find-id-file id-prop id no-fallback))
        org-agenda-new-buffers where)
    (when file
      (setq where (org-generic-id-find-id-in-file id-prop id file markerp)))
    (unless (or where cached)
      (org-generic-id-update-id-locations id-prop nil t)
      (setq file (org-generic-id-find-id-file id-prop id no-fallback))
      (when file
        (setq where (org-generic-id-find-id-in-file
                     id-prop id file markerp))))
    where))

;;; Internal functions

;; Storing ID locations (files)

;;;###autoload
(defun org-generic-id-update-id-locations (id-prop &optional files silent)
  "Scan relevant files for IDs.
Store the relation between files and corresponding IDs.
This will scan all agenda files, all associated archives, and all
files currently mentioned in `org-generic-id-locations'.
When FILES is given, scan also these files."
  (interactive "sID Property: ")
  (let* (id-locations
         (files
          (delete-dups
           (mapcar #'file-truename
                   (cl-remove-if-not
                    ;; Default `org-generic-id-extra-files' value contains
                    ;; `agenda-archives' symbol.
                    #'stringp
                    (append
                     ;; Agenda files and all associated archives.
                     (org-agenda-files t org-generic-id-search-archives)
                     ;; Explicit extra files.
                     (if (symbolp org-generic-id-extra-files)
                         (symbol-value org-generic-id-extra-files)
                       org-generic-id-extra-files)
                     ;; All files known to have IDs.
                     org-generic-id-files
                     ;; Additional files from function call.
                     files)))))
         (nfiles (length files))
         (id-regexp
          (rx-to-string `(seq bol (0+ (any "\t "))
                              ,(format ":%s:" id-prop)
                              (1+ " ") (not (any " ")))))
         (seen-ids nil)
         (ndup 0)
         (i 0))
    (with-temp-buffer
      (delay-mode-hooks
        (org-mode)
        (dolist (file files)
          (when (file-exists-p file)
            (unless silent
              (cl-incf i)
              (message "Finding :%s: locations (%d/%d files): %s"
                       id-prop i nfiles file))
            (insert-file-contents file nil nil nil 'replace)
            (let ((ids nil)
                  (case-fold-search t))
              (org-with-point-at 1
                (while (re-search-forward id-regexp nil t)
                  (when (org-at-property-p)
                    (push (org-entry-get (point) id-prop) ids)))
                (when ids
                  (push (cons (abbreviate-file-name file) ids)
                        id-locations)
                  (dolist (id ids)
                    (cond
                     ((not (member id seen-ids)) (push id seen-ids))
                     (silent nil)
                     (t
                      (message "Duplicate :%s: property %S" id-prop id)
                      (cl-incf ndup)))))))))))
    (puthash id-prop
             (org-generic-id--alist-to-hash id-locations)
             org-generic-id-locations)
    ;; Save the new locations and reload to regenerate variables.
    (org-generic-id-locations-save)
    (org-generic-id-locations-load)
    (when (and (not silent) (> ndup 0))
      (warn
       "WARNING: %d duplicate :%s: properties found, check *Messages* buffer"
       ndup id-prop))
    (message "%d files scanned, %d files contain IDs, and %d :%s: IDs found."
             nfiles (length org-generic-id-files)
             (hash-table-count (gethash id-prop org-generic-id-locations))
             id-prop)
    org-generic-id-locations))

(defun org-generic-id-locations-save ()
  "Save `org-generic-id-locations' in `org-generic-id-locations-file'."
  (when (and org-generic-id-locations)
    (let ((out (if (hash-table-p org-generic-id-locations)
                   (org-generic-id--locations-hash-to-alist
                    org-generic-id-locations)
                 org-generic-id-locations)))
      (when (and org-generic-id-locations-file-relative out)
        (setq out (mapcar
                   (lambda (item)
                     (if (file-name-absolute-p (car item))
                         (cons (file-relative-name
                                (car item) (file-name-directory
                                            org-generic-id-locations-file))
                               (cdr item))
                       item))
                   out)))
      (with-temp-file org-generic-id-locations-file
        (let ((print-level nil)
              (print-length nil))
          (pp out (current-buffer)))))))

;;;###autoload
(defun org-generic-id-locations-load ()
  "Read the data from `org-generic-id-locations-file'."
  (setq org-generic-id-locations nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents org-generic-id-locations-file)
          (setq org-generic-id-locations (read (current-buffer)))
          (let ((loc (file-name-directory org-generic-id-locations-file)))
            (mapc (lambda (id-item)
                    (mapc
                     (lambda (item)
                       (unless (file-name-absolute-p (car item))
                         (setf (car item) (expand-file-name (car item) loc))))
                     (cdr id-item)))
                  org-generic-id-locations)))
      (error
       (message "Could not read org-generic-id-values from %s.  Setting it to nil."
                org-generic-id-locations-file))))
  (setq org-generic-id-files
        (apply #'append
               (mapcar (lambda (x) (mapcar #'car (cdr x)))
                       org-generic-id-locations)))
  (setq org-generic-id-locations (org-generic-id--locations-alist-to-hash org-generic-id-locations)))

;;;###autoload
(defun org-generic-id-add-location (id-prop id file)
  "Add the ID with location FILE to the database of ID locations."
  ;; Only when the buffer has a file
  (unless file
    (error "bug: ‘org-generic-id-add-locations' expects a file-visiting buffer"))
  (let ((afile (abbreviate-file-name file)))
    (when (and id)
      (let ((id-prop-hash (gethash id-prop org-generic-id-locations
                                   (make-hash-table :test 'equal))))
        (puthash id-prop id-prop-hash org-generic-id-locations)
        (puthash id afile id-prop-hash))
      (unless (member afile org-generic-id-files)
        (add-to-list 'org-generic-id-files afile)))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'org-generic-id-locations-save))

(defun org-generic-id--locations-hash-to-alist (hash)
  "Turn an org-generic-id hash into an alist, so it can be written to a file."
  (let (res)
    (maphash
     (lambda (k v)
       (let ((v (org-generic-id--hash-to-alist v)))
         (setf (alist-get k res) v)))
     hash)
    (setq res (sort res (lambda (x y) (string< (car x) (car y)))))
    res))

(defun org-generic-id--hash-to-alist (hash)
  "Turn a hash into an alist while reversing keys and values.

Create an alist with each key being the distinct values in the original hash,
and each value a list of all original hash keys that map to the key of the
alist. For example, a hash

    {’id1’: ’file1’, ’id2’: ’file2’, ’id3’: ’file1’}

is turned into an alist like this:

    ((’file1’ . (’id1’ ’id3’)) (’file2’ . (’id2’)))"
  (let (res x)
    (maphash
     (lambda (k v)
       (if (setq x (assoc v res))
           (setcdr x (cons k (cdr x)))
         (push (list v k) res)))
     hash)
    (mapc (lambda (x)
            (setcdr x (sort (cdr x) #'string<)))
          res)
    (setq res (sort res (lambda (x y) (string< (car x) (car y)))))
    res))

(defun org-generic-id--locations-alist-to-hash (list)
  "Turn an org-generic-id location list into a hash table."
  (let ((res (make-hash-table
              :test 'equal
              :size (apply '+ (mapcar 'length list)))))
    (mapc
     (lambda (x)
       (puthash (car x) (org-generic-id--alist-to-hash (cdr x)) res))
     list)
    res))

(defun org-generic-id--alist-to-hash (list)
  "Reverse the transformation made in ‘org-generic-id--hash-to-alist’."
  (let ((res (make-hash-table
              :test 'equal
              :size (apply '+ (mapcar 'length list))))
        f)
    (mapc
     (lambda (x)
       (setq f (car x))
       (mapc (lambda (i) (puthash i f res)) (cdr x)))
     list)
    res))

;; Finding entries with specified id

;;;###autoload
(defun org-generic-id-find-id-file (id-prop id &optional no-fallback)
  "Query the id database for the file in which this ID is located.

If NO-FALLBACK is set, don’t fall back to current buffer if not found in
‘org-generic-id-locations’."
  (or (and org-generic-id-locations
           (hash-table-p org-generic-id-locations)
           ;; Guard for errors in the ‘gethash’ call after this
           (gethash id-prop org-generic-id-locations)
           (gethash id
                    (gethash id-prop org-generic-id-locations)))
      ;; Fall back on current buffer
      (and (not no-fallback)
           (buffer-file-name (or (buffer-base-buffer (current-buffer))
                                 (current-buffer))))))

(defun org-generic-id-find-id-in-file (id-prop id file &optional markerp)
  "Return the position of the entry ID in FILE.

If that files does not exist, or if it does not contain this ID,
return nil.

The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a new marker."
  (cond
   ((not file) nil)
   ((not (file-exists-p file)) nil)
   (t
    (let* ((visiting (find-buffer-visiting file))
           (buffer (or visiting (find-file-noselect file))))
      (unwind-protect
          (with-current-buffer buffer
            (let ((pos (org-with-wide-buffer (org-find-property id-prop id))))
              (cond
               ((null pos) nil)
               (markerp (move-marker (make-marker) pos buffer))
               (t (cons file pos)))))
        ;; Remove opened buffer in the process.
        (unless (or visiting markerp) (kill-buffer buffer)))))))

(unless (featurep 'org-generic-id)
  (unless org-generic-id-locations
    (message "Loading org-generic-id-locations on first load.")
    (org-generic-id-locations-load)))

(provide 'org-generic-id)

;;; org-generic-id.el ends here

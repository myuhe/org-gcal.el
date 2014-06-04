;;; org-gcal.el --- Org sync with Google Calendar -*- lexical-binding: t -*-

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/org-gcal.el
;; Version: 0.1
;; Maintainer: myuhe
;; Copyright (C) :2014 myuhe all rights reserved.
;; Created: :14-01-03
;; Package-Requires: ((request-deferred "0.2.0") (alert "1.1") (emacs "24") (cl-lib "0.5") (org "8.2.4"))
;; Keywords: convenience,

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published byn
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:
;;
;; Put the org-gcal.el to your
;; load-path.
;; Add to .emacs:
;; (require 'org-gcal)
;;
;;; Changelog:
;; 2014-01-03 Initial release.

(require 'alert)
(require 'json)
(require 'request-deferred)
(require 'org-element)
(require 'org-archive)
(require 'cl-lib)

;; Customization
(defgroup org-gcal nil "Org sync with Google Calendar"
  :tag "Org google calendar"
  :group 'org)

(defcustom org-gcal-up-days 30
  "Number of days to get events before today"
  :group 'org-gcal
  :type 'integer)

(defcustom org-gcal-down-days 60
  "Number of days to get events after today"
  :group 'org-gcal
  :type 'integer)

(defcustom org-gcal-auto-archive t
  "If non-nil, old events archive automatically."
  :group 'org-gcal
  :type 'boolean)

(defcustom org-gcal-dir
  (concat user-emacs-directory "org-gcal/")
  "File in which to save token."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-token-file
  (expand-file-name ".org-gcal-token" org-gcal-dir)
  "File in which to save token."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-client-id nil
  "Client ID for OAuth."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-client-secret nil
  "Google calendar secret key for OAuth."
  :group 'org-gcal
  :type 'string)

(defcustom org-gcal-file-alist nil
  "list of association '(calendar-id file) to synchronize at once
   for calendar id."
  :group 'org-gcal
  :type '(repeat (list :tag "Calendar file" (string :tag "Calendar Id") (file :tag "Org file"))))

(defcustom org-gcal-logo "org.png"
  "org-gcal logo filename"
  :group 'org-gcal
  :type `(choice  ,@(mapcar (lambda (c)
                       `(const :tag ,c ,c))
                     '("org.png" "emacs.png"))))

(defvar org-gcal-token-plist nil
  "token plist")

(defvar org-gcal-icon-list '("org.png" "emacs.png")
  "icon file name list")

(defconst org-gcal-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst org-gcal-token-url "https://accounts.google.com/o/oauth2/token"
  "Google OAuth2 server URL.")

(defconst org-gcal-resource-url "https://www.googleapis.com/auth/calendar"
  "URL used to request access to calendar resources.")

(defconst org-gcal-key-url (concat "?key=" org-gcal-client-secret))

(defconst org-gcal-events-url "https://www.googleapis.com/calendar/v3/calendars/%s/events")

(defun org-gcal-fetch (&optional a-token)
  (interactive)
  (org-gcal--ensure-token)
  (when org-gcal-auto-archive
    (dolist (i org-gcal-file-alist)
      (with-current-buffer
          (find-file-noselect (cdr i))
        (org-gcal--archive-old-event))
      (kill-buffer (get-file-buffer (cdr i)))))
  (cl-loop for x in org-gcal-file-alist
           do
           (let ((x x)
                         (a-token (if a-token
                                      a-token
                                    (org-gcal--get-access-token))))
             (deferred:$
               (request-deferred
                (format org-gcal-events-url (car x))
                :type "GET"
                :params `((access_token . ,a-token)
                          (key . ,org-gcal-client-secret)
                          (singleEvents . "True")
                          (timeMin . ,(org-gcal--subsract-time))
                          (timeMax . ,(org-gcal--add-time))
                          ("grant_type" . "authorization_code"))
                :parser 'org-gcal--json-read
                :error
                (cl-function (lambda (&key error-thrown)
                             (message "Got error: %S" error-thrown))))
               (deferred:nextc it
                 (lambda (response)
                   (let
                       ((temp (request-response-data response))
                        (status (request-response-status-code response))
                        (error-msg (request-response-error-thrown response)))
                     (cond
                      ;; If there is no network connectivity, the response will
                      ;; not include a status code.
                      ((eq status nil)
                       (org-gcal--notify
                        "Got Error"
                        "Could not contact remote service. Please check your network connectivity."))
                      ;; Receiving a 403 response could mean that the calendar
                      ;; API has not been enabled. When the user goes and
                      ;; enables it, a new token will need to be generated. This
                      ;; takes care of that step.
                      ((eq 401 status)
                       (progn
                         (org-gcal--notify
                          "Received HTTP 401"
                          "OAuth token expired. Now trying to refresh-token")
                         (org-gcal-refresh-token 'org-gcal-fetch)))
                      ((eq 403 status)
                       (progn
                         (org-gcal--notify "Received HTTP 403"
                                           "Ensure you enabled the Calendar API through the Developers Console, then try again.")
                         (org-gcal-refresh-token)))
                      ;; We got some 2xx response, but for some reason no
                      ;; message body.
                      ((and (> 299 status) (eq temp nil))
                       (org-gcal--notify 
                        (concat "Received HTTP" (number-to-string status)) 
                        "Error occured, but no message body."))
                      ((not (eq error-msg nil))
                       ;; Generic error-handler meant to provide useful
                       ;; information about failure cases not otherwise
                       ;; explicitly specified.
                       (progn
                         (org-gcal--notify 
                          (concat "Status code: " (number-to-string status))
                          error-msg)))
                      ;; Fetch was successful.
                      (t (progn
                           (org-gcal--notify "Completed event fetching ." (concat "Fetched data overwrote\n" (cdr x)))
                           (write-region
                            (mapconcat 'identity
                                       (mapcar (lambda (lst)
                                                 (org-gcal--cons-list lst))
                                               (plist-get (request-response-data response) :items ))
                                       "")
                            nil (cdr x) nil 'nomsg)))))))))))

(defun org-gcal-post-at-point ()
  (interactive)
  (org-gcal--ensure-token)
  (save-excursion
    (end-of-line)
    (re-search-backward org-heading-regexp)
    (re-search-forward "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
    (backward-char 11)
    (let* ((elem (org-element-headline-parser (point-max) t))
           (tobj (org-element-timestamp-parser))
           (smry (org-element-property :title elem))
           (loc  (org-element-property :LOCATION elem))
           (id  (org-element-property :ID elem))
           (start (org-gcal--format-org2iso
                   (plist-get (cadr tobj) :year-start)
                   (plist-get (cadr tobj) :month-start)
                   (plist-get (cadr tobj) :day-start)
                   (plist-get (cadr tobj) :hour-start)
                   (plist-get (cadr tobj) :minute-start)
                   (when (plist-get (cadr tobj) :hour-start)
                     t)))
           (end (org-gcal--format-org2iso
                 (plist-get (cadr tobj) :year-end)
                 (plist-get (cadr tobj) :month-end)
                 (plist-get (cadr tobj) :day-end)
                 (plist-get (cadr tobj) :hour-end)
                 (plist-get (cadr tobj) :minute-end)
                 (when (plist-get (cadr tobj) :hour-start)
                   t)))
           (desc  (if (plist-get (cadr elem) :contents-begin)
                      (replace-regexp-in-string
                       " *:PROPERTIES:\n  \\(.*\\(?:\n.*\\)*?\\) :END:\n" ""
                       (buffer-substring-no-properties
                        (plist-get (cadr elem) :contents-begin)
                        (plist-get (cadr elem) :contents-end))) "")))
      (org-gcal--post-event start end smry loc desc id))))

(defun org-gcal-request-authorization ()
  "Request OAuth authorization at AUTH-URL by launching `browse-url'.
CLIENT-ID is the client id provided by the provider.
It returns the code provided by the service."
  (browse-url (concat org-gcal-auth-url
                      "?client_id=" (url-hexify-string org-gcal-client-id)
                      "&response_type=code"
                      "&redirect_uri=" (url-hexify-string "urn:ietf:wg:oauth:2.0:oob")
                      "&scope=" (url-hexify-string org-gcal-resource-url)))
  (read-string "Enter the code your browser displayed: "))

(defun org-gcal-request-token ()
  "Request OAuth access at TOKEN-URL."
  (request
   org-gcal-token-url
   :type "POST"
   :data `(("client_id" . ,org-gcal-client-id)
           ("client_secret" . ,org-gcal-client-secret)
           ("code" . ,(org-gcal-request-authorization))
           ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
           ("grant_type" . "authorization_code"))
   :parser 'org-gcal--json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (when data
                 (setq org-gcal-token-plist data)
                 (org-gcal--save-sexp data org-gcal-token-file))))
   :error
   (cl-function (lambda (&key error-thrown)
                (message "Got error: %S" error-thrown)))))

(defun org-gcal-refresh-token (&optional fun start end smry loc desc id)
  "Refresh OAuth access at TOKEN-URL."
  (interactive)
    (deferred:$
      (request-deferred
       org-gcal-token-url
       :type "POST"
       :data `(("client_id" . ,org-gcal-client-id)
               ("client_secret" . ,org-gcal-client-secret)
               ("refresh_token" . ,(org-gcal--get-refresh-token))
               ("grant_type" . "refresh_token"))
       :parser 'org-gcal--json-read
       :error
       (cl-function (lambda (&key error-thrown)
                    (message "Got error: %S" error-thrown))))
      (deferred:nextc it
        (lambda (response)
          (let ((temp (request-response-data response)))
            (plist-put org-gcal-token-plist
                       :access_token
                       (plist-get temp :access_token))
            (org-gcal--save-sexp org-gcal-token-plist org-gcal-token-file)
            org-gcal-token-plist)))
      (deferred:nextc it
        (lambda (token)
          (cond ((eq fun 'org-gcal-fetch)
                 (org-gcal-fetch (plist-get token :access_token)))
                ((eq fun 'org-gcal--post-event)
                 (org-gcal--post-event start end smry loc desc id (plist-get token :access_token))))))))

;; Internal
(defun org-gcal--archive-old-event ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (goto-char (cdr (org-gcal--timestamp-successor)))
      (let ((elem (org-element-headline-parser (point-max) t))
            (tobj (cadr (org-element-timestamp-parser))))
        (when (>
               (time-to-seconds (time-subtract (current-time) (days-to-time org-gcal-up-days)))
               (time-to-seconds (encode-time 0  (if (plist-get tobj :minute-start)
                                                    (plist-get tobj :minute-start) 0)
                                             (if (plist-get tobj :hour-start)
                                                 (plist-get tobj :hour-start) 0)
                                             (plist-get tobj :day-start)
                                             (plist-get tobj :month-start)
                                             (plist-get tobj :year-start))))
          (org-gcal--notify "Archived event." (org-element-property :title elem))
          (org-archive-subtree))))
    (save-buffer)))

(defun org-gcal--save-sexp (data file)
  (if (file-directory-p org-gcal-dir)
      (if (file-exists-p file)
          (progn
            (with-temp-file file
              (pp data (current-buffer))))
        (progn
          (find-file-noselect file)
          (with-temp-file file
            (pp data (current-buffer)))))
    (progn
      (make-directory org-gcal-dir)
      (org-gcal--save-sexp data file))))

(defun org-gcal--json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "^{" nil t)
    (delete-region (point-min) (1- (point)))
    (goto-char (point-min))
    (json-read)))

(defun org-gcal--get-refresh-token ()
  (if org-gcal-token-plist
      (plist-get org-gcal-token-plist :refresh_token)
    (progn
      (if (file-exists-p org-gcal-token-file)
          (progn
            (with-temp-buffer (insert-file-contents org-gcal-token-file)
                              (plist-get (read (buffer-string)) :refresh_token)))
        (org-gcal--notify 
         (concat org-gcal-token-file " is not exists" )
         (concat "Make" org-gcal-token-file))))))

(defun org-gcal--get-access-token ()
  (if org-gcal-token-plist
      (plist-get org-gcal-token-plist :access_token)
    (progn
      (if (file-exists-p org-gcal-token-file)
          (progn
            (with-temp-buffer (insert-file-contents org-gcal-token-file)
                              (plist-get (read (buffer-string)) :access_token)))
        (org-gcal--notify 
         (concat org-gcal-token-file " is not exists" )
         (concat "Make" org-gcal-token-file))))))

(defun org-gcal--safe-substring (string from &optional to)
  "Calls the `substring' function safely.
\nNo errors will be returned for out of range values of FROM and
TO.  Instead an empty string is returned."
  (let* ((len (length string))
         (to (or to len)))
    (when (< from 0)
      (setq from (+ len from)))
    (when (< to 0)
      (setq to (+ len to)))
    (if (or (< from 0) (> from len)
            (< to 0) (> to len)
            (< to from))
        ""
      (substring string from to))))

(defun org-gcal--alldayp (s e)
  (let ((slst (org-gcal--parse-date s))
        (elst (org-gcal--parse-date e)))
    (and
     (= (length s) 10)
     (= (length e) 10)
     (= (- (time-to-seconds
            (encode-time 0 0 0
                         (plist-get elst :day)
                         (plist-get elst :mon)
                         (plist-get elst :year)))
           (time-to-seconds
            (encode-time 0 0 0
                         (plist-get slst :day)
                         (plist-get slst :mon)
                         (plist-get slst :year)))) 86400))))

(defun org-gcal--parse-date (str)
  (list :year (string-to-number  (org-gcal--safe-substring str 0 4))
        :mon  (string-to-number (org-gcal--safe-substring str 5 7))
        :day  (string-to-number (org-gcal--safe-substring str 8 10))
        :hour (string-to-number (org-gcal--safe-substring str 11 13))
        :min  (string-to-number (org-gcal--safe-substring str 14 16))
        :sec  (string-to-number (org-gcal--safe-substring str 17 19))))

(defun org-gcal--adjust-date (fn day)
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (funcall fn (current-time) (days-to-time day))))

(defun org-gcal--add-time ()
  (org-gcal--adjust-date 'time-add org-gcal-down-days))

(defun org-gcal--subsract-time ()
  (org-gcal--adjust-date 'time-subtract org-gcal-up-days))

(defun org-gcal--format-iso2org (str &optional tz)
  (let ((plst (org-gcal--parse-date str)))
    (concat
     "<"
     (format-time-string
      (if (< 11 (length str)) "%Y-%m-%d %a %H:%M" "%Y-%m-%d %a")
      (seconds-to-time
       (+ (if tz (car (current-time-zone)) 0)
          (time-to-seconds
           (encode-time
            (plist-get plst :sec)
            (plist-get plst :min)
            (plist-get plst :hour)
            (plist-get plst :day)
            (plist-get plst :mon)
            (plist-get plst :year))))))
     ;;(if (and repeat (not (string= repeat ""))) (concat " " repeat) "")
     ">")))

(defun org-gcal--format-org2iso (year mon day &optional hour min tz)
  (concat
   (format-time-string
    (if (or hour min) "%Y-%m-%dT%H:%M" "%Y-%m-%d")
    (seconds-to-time
     (-
      (time-to-seconds
       (encode-time 0
                    (if min min 0)
                    (if hour hour 0)
                    day mon year))
      (if tz
          (car (current-time-zone)) 0))))
   (when (or hour min) ":00z")))

(defun org-gcal--iso-next-day (str &optional previous-p)
  (let ((format (if (< 11 (length str))
                    "%Y-%m-%dT%H:%M"
                  "%Y-%m-%d"))
        (plst (org-gcal--parse-date str))
        (prev (if previous-p -1 +1)))
    (format-time-string format
                        (seconds-to-time
                         (+ (time-to-seconds
                             (encode-time
                              (plist-get plst :sec)
                              (plist-get plst :min)
                              (plist-get plst :hour)
                              (plist-get plst :day)
                              (plist-get plst :mon)
                              (plist-get plst :year)))
                            (* 60 60 24 prev))))))

(defun org-gcal--iso-previous-day (str)
  (org-gcal--iso-next-day str t))

(defun org-gcal--cons-list (plst)
  (let* ((smry  (plist-get plst :summary))
         (desc  (plist-get plst :description))
         (loc   (plist-get plst :location))
         (link  (plist-get plst :htmlLink))
         (id    (plist-get plst :id))
         (stime (plist-get (plist-get plst :start)
                           :dateTime))
         (etime (plist-get (plist-get plst :end)
                           :dateTime))
         (sday  (plist-get (plist-get plst :start)
                           :date))
         (eday  (plist-get (plist-get plst :end)
                           :date))
         (start (if stime stime sday))
         (end   (if etime etime eday)))
    (concat
     "* " smry
     (if (or (string= start end) (org-gcal--alldayp start end))
         (concat "\n  "(org-gcal--format-iso2org start))
       (if (and
            (= (plist-get (org-gcal--parse-date start) :year)
               (plist-get (org-gcal--parse-date end)   :year))
            (= (plist-get (org-gcal--parse-date start) :mon)
               (plist-get (org-gcal--parse-date end)   :mon))
            (= (plist-get (org-gcal--parse-date start) :day)
               (plist-get (org-gcal--parse-date end)   :day)))
           (concat "\n  <"
                   (org-gcal--format-date start "%Y-%m-%d %a %H:%M")
                   "-"
                   (org-gcal--format-date end "%H:%M")
                   ">")
         (concat "\n  " (org-gcal--format-iso2org start)
                 "--"
                 (org-gcal--format-iso2org
                  (if (< 11 (length end))
                      end
                    (org-gcal--iso-previous-day end)))))) "\n\n"
                 desc (when desc "\n")
                 "  :PROPERTIES:\n"
                 "  :LOCATION: " loc "\n"
                 "  :LINK: ""[[" link "][Go to gcal web page]]\n"
                 "  :ID: " id "\n"
                 "  :END:\n\n")))

(defun org-gcal--format-date (str format &optional tz)
  (let ((plst (org-gcal--parse-date str)))
    (concat
     (format-time-string format
                         (seconds-to-time
                          (+ (if tz (car (current-time-zone)) 0)
                             (time-to-seconds
                              (encode-time
                               (plist-get plst :sec)
                               (plist-get plst :min)
                               (plist-get plst :hour)
                               (plist-get plst :day)
                               (plist-get plst :mon)
                               (plist-get plst :year)))))))))

(defun org-gcal--param-date (str)
  (if (< 11 (length str)) "dateTime" "date"))

(defun org-gcal--post-event (start end smry loc desc &optional id a-token)
  (let ((stime (org-gcal--param-date start))
                (etime (org-gcal--param-date end))
                (a-token (if a-token
                             a-token
                           (org-gcal--get-access-token))))
    (request
     (concat
      (format org-gcal-events-url (car (car org-gcal-file-alist)))
      (when id
        (concat "/" id)))
     :type (if id "PATCH" "POST")
     :headers '(("Content-Type" . "application/json"))
     :data (json-encode `(("start"  (,stime . ,start))
                          ("end"  (,etime . ,(if (equal "date" etime)
                                                 (org-gcal--iso-next-day end)
                                               end)))
                          ("summary" . ,smry)
                          ("location" . ,loc)
                          ("description" . ,desc)))
     :params `((access_token . ,a-token)
               (key . ,org-gcal-client-secret)
               ("grant_type" . "authorization_code"))

     :parser 'org-gcal--json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (if  (string=
                       (plist-get (plist-get data :error) :message)
                       "Invalid Credentials")
                     (progn
                       (org-gcal-refresh-token 'org-gcal--post-event))
                   (progn
                     (org-gcal--notify "Event Posted"
                                       (concat "Org-gcal post event\n  " (plist-get data :summary)))
                     (org-gcal-fetch))))))))

(defun org-gcal--capture-post ()
  (dolist (i org-gcal-file-alist)
    (when (string=  (file-name-nondirectory (cdr i))
                    (substring (buffer-name) 8))
      (org-gcal-post-at-point))))

(add-hook 'org-capture-before-finalize-hook 'org-gcal--capture-post)

(defun org-gcal--ensure-token ()
  (cond
   (org-gcal-token-plist t)
   ((and (file-exists-p org-gcal-token-file)
         (ignore-errors
           (setq org-gcal-token-plist
                 (with-temp-buffer
                   (insert-file-contents org-gcal-token-file)
                   (read (current-buffer)))))) t)
   (t (org-gcal-request-token))))

(defun org-gcal--timestamp-successor ()
  "Search for the next timestamp object.
Return value is a cons cell whose CAR is `timestamp' and CDR is
beginning position."
  (save-excursion
    (when (re-search-forward
           (concat org-ts-regexp-both
                   "\\|"
                   "\\(?:<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
                   "\\|"
                   "\\(?:<%%\\(?:([^>\n]+)\\)>\\)")
           nil t)
      (cons 'timestamp (match-beginning 0)))))

(defun org-gcal--notify (title mes)
  (let ((file (expand-file-name (concat org-gcal-dir org-gcal-logo)))
                (mes mes)
                (title title))
    (if (file-exists-p file)
        (if (eq system-type 'gnu/linux)
            (alert mes :title title :icon file)
          (alert mes :title title))
      (deferred:$
        (deferred:url-retrieve (concat "https://raw.githubusercontent.com/myuhe/org-gcal.el/master/" org-gcal-logo))
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
             (let ((tmp (substring (buffer-string) (+ (string-match "\n\n" (buffer-string)) 2))))
               (erase-buffer)
               (insert tmp)
               (write-file file)))
            (kill-buffer buf)
            (if (eq system-type 'gnu/linux)
            (alert mes :title title :icon file)
          (alert mes :title title))
            ))))))

(provide 'org-gcal)

;;; org-gcal.el ends here

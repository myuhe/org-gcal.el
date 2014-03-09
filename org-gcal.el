;;; org-gcal.el --- Org sync with Google Calendar

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/org-gcal.el
;; Version: 0.1
;; Maintainer: myuhe
;; Copyright (C) :2014 myuhe all rights reserved.
;; Created: :14-01-03
;; Package-Requires: ((request "0.2.0") (gntp "0.1"))
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

(require 'json)
(require 'request-deferred)
(require 'org-element)
(require 'org-archive)
(require 'gntp)
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

(defcustom org-gcal-use-notifications nil
  "If non-nil notify via `notifications-notify' instead of `gntp-notify'."
  :group 'org-gcal
  :type 'boolean)

(defcustom org-gcal-logo "http://raw.github.com/myuhe/org-gcal.el/master/org-gcal-logo.png"
  "org-gcal logo filename or URL."
  :group 'org-gcal
  :type 'string)

(defvar org-gcal-token-plist nil
  "token plist")

(defconst org-gcal-auth-url "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst org-gcal-token-url "https://accounts.google.com/o/oauth2/token"
  "Google OAuth2 server URL.")

(defconst org-gcal-resource-url "https://www.googleapis.com/auth/calendar"
  "URL used to request access to calendar resources.")

(defconst org-gcal-key-url (concat "?key=" org-gcal-client-secret))

(defconst org-gcal-events-url "https://www.googleapis.com/calendar/v3/calendars/%s/events")

(defun org-gcal-fetch ()
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
           (lexical-let ((x x)) 
             (deferred:$       
               (request-deferred
                (format org-gcal-events-url (car x))
                :type "GET"        
                :params `((access_token . ,(org-gcal--get-access-token))
                          (key . ,org-gcal-client-secret)
                          (singleEvents . "True")
                          (timeMin . ,(org-gcal--subsract-time))
                          (timeMax . ,(org-gcal--add-time))
                          ("grant_type" . "authorization_code"))
                :parser 'org-gcal--json-read)
               (deferred:nextc it
                 (lambda (response)
                   (let ((temp (request-response-data response)))
                     (org-gcal--notify "Completed event fetching ." (concat "Fetched data overwrote\n\" (cdr x)) org-gcal-logo)
                     (write-region
                      (mapconcat 'identity
                                 (mapcar (lambda (lst) 
                                           (org-gcal--cons-list lst))
                                         (plist-get (request-response-data response) :items ))
                                 "")
                      nil (cdr x)))))))))

(defun org-gcal-fetch-test ()
  (interactive)
  (cl-loop for x in org-gcal-file-alist
           do
           (lexical-let ((x x))
             (deferred:$
               (request-deferred
                (format org-gcal-events-url (car x))
                :type "GET"
                :params `((access_token . ,(org-gcal--get-access-token))
                          (key . ,org-gcal-client-secret)
                          (singleEvents . "True")
                          (timeMin . ,(org-gcal--subsract-time))
                          (timeMax . ,(org-gcal--add-time))
                          ("grant_type" . "authorization_code"))
                :parser 'buffer-string)
               (deferred:nextc it
                 (lambda (response)
                   (let ((temp (request-response-data response)))
                     (with-current-buffer (get-buffer-create "*request demo*")
                       (erase-buffer)
                       (insert (request-response-data response))
                       (pop-to-buffer (current-buffer))))))))))

(defun org-gcal-post-at-point ()
  (interactive)
  (org-gcal--ensure-token)
  (save-excursion
    (end-of-line)
    (re-search-backward org-heading-regexp)
    (re-search-forward "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
    (backward-char 11)
    (let* ((elem (org-element-headline-parser nil t))
           (tobj (org-element-timestamp-parser))
           (smry (org-element-property :title elem))
           (loc  (org-element-property :LOCATION elem))
           (id  (org-element-property :ID elem))
           (start (org-gcal--format-org2iso
                   (plist-get (cadr tobj) :year-start)
                   (plist-get (cadr tobj) :month-start)
                   (plist-get (cadr tobj) :day-start)
                   (plist-get (cadr tobj) :hour-start)
                   (plist-get (cadr tobj) :minute-start)))
           (end (org-gcal--format-org2iso
                 (plist-get (cadr tobj) :year-end)
                 (plist-get (cadr tobj) :month-end)
                 (plist-get (cadr tobj) :day-end)
                 (plist-get (cadr tobj) :hour-end)
                 (plist-get (cadr tobj) :minute-end)))
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
   :success (function* 
             (lambda (&key data &allow-other-keys)
               (when data
                 (setq org-gcal-token-plist data)
                 (org-gcal--save-sexp data org-gcal-token-file))))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                (message "Got error: %S" error-thrown)))))

(defun org-gcal-refresh-token ()
  "Refresh OAuth access at TOKEN-URL."
  (interactive)  
  (request 
   org-gcal-token-url
   :type "POST"        
   :data `(("client_id" . ,org-gcal-client-id)
           ("client_secret" . ,org-gcal-client-secret)
           ("refresh_token" . ,(org-gcal--get-refresh-token))
           ("grant_type" . "refresh_token"))
   :parser 'org-gcal--json-read
   :success (function* 
             (lambda (&key data &allow-other-keys)
               (when data
                 (plist-put org-gcal-token-plist
                            ':access_token 
                            (plist-get data ':access_token))
                 (org-gcal--save-sexp org-gcal-token-plist org-gcal-token-file))))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                (message "Got error: %S" error-thrown)))))

;; Internal
(defun org-gcal--archive-old-event ()
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
  (goto-char (cdr (org-element-timestamp-successor)))
  (when (>
    (time-to-seconds (time-subtract (current-time) (days-to-time org-gcal-up-days)))
    (time-to-seconds (encode-time 0  (if (plist-get (cadr (org-element-timestamp-parser)) :minute-start)
                    (plist-get (cadr (org-element-timestamp-parser)) :minute-start) 0)
             (if (plist-get (cadr (org-element-timestamp-parser)) :hour-start)
                 (plist-get (cadr (org-element-timestamp-parser)) :hour-start) 0)
                   (plist-get (cadr (org-element-timestamp-parser)) :day-start)
                   (plist-get (cadr (org-element-timestamp-parser)) :month-start)
                   (plist-get (cadr (org-element-timestamp-parser)) :year-start))))
      (org-advertized-archive-subtree))))

(defun org-gcal--save-sexp (data file)
  (if (file-directory-p org-gcal-dir)
      (if (file-exists-p file)
        (with-temp-file file
          (insert (pp data)))
        (progn
          (find-file-noselect file)
          (with-temp-file file
            (insert (pp data)))
          (kill-buffer (get-file-buffer file))))
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
  (if (file-exists-p org-gcal-token-file)
      (progn
      (with-temp-buffer (insert-file-contents org-gcal-token-file)
        (plist-get (read (buffer-string)) :refresh_token)))
         (message "\"%s\" is not exists" org-gcal-token-file)))

(defun org-gcal--get-access-token ()
  (if (file-exists-p org-gcal-token-file)
      (progn
      (with-temp-buffer (insert-file-contents org-gcal-token-file)
        (plist-get (read (buffer-string)) :access_token)))
    (message "\"%s\" is not exists" org-gcal-token-file)))
    
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

(defun org-gcal--format-org2iso (year mon day &optional hour min) 
  (concat
   (format-time-string 
    (if (or hour min) "%Y-%m-%dT%H:%M" "%Y-%m-%d")
    (seconds-to-time
     (time-to-seconds
      (encode-time 0 
                   (if min min 0)
                   (if hour hour 0)
                   day mon year))))
   (when (or hour min) ":00z")))

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
     (if (org-gcal--alldayp start end)
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
                 (org-gcal--format-iso2org end)))) "\n\n"
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

(defun org-gcal--post-event (start end smry loc desc &optional id)
  (org-gcal--ensure-token)
           (lexical-let ((stime (org-gcal--param-date start))
                         (etime (org-gcal--param-date end)))
             (request
              (concat
               (format org-gcal-events-url (car (car org-gcal-file-alist)))
               (when id 
                 (concat "/" id)))
              :type (if id "PATCH" "POST")
              :headers '(("Content-Type" . "application/json"))
              :data (json-encode `(("start"  (,stime . ,start))  
                                   ("end"  (,etime . ,end))
                                   ("summary" . ,smry)
                                   ("location" . ,loc)
                                   ("description" . ,desc)))
              :params `((access_token . ,(org-gcal--get-access-token))
                        (key . ,org-gcal-client-secret)
                        ("grant_type" . "authorization_code"))

              :parser 'org-gcal--json-read
              :success (function*
                        (lambda (&key data &allow-other-keys)
                           (message "I sent: %S" data)
                          (org-gcal--notify "Event Posted"
                                            (concat "Org-gcal post event\n  " (plist-get data :summary))
                                            org-gcal-logo))))))

(defun org-gcal--ensure-token ()
  (cond
   (org-gcal-token-plist t)
   ((and (file-exists-p org-gcal-token-file)
         (ignore-errors
           (setq org-gcal-token-plist 
                 (with-temp-buffer
                   (insert-file-contents org-gcal-token-file)
                   (read (current-buffer))))))
    t)
   (t
    (org-gcal-request-token))))

(defun org-gcal--notify (title mes &optional icn)
  (if org-gcal-use-notifications
      (notifications-notify :title title
                            :body mes)
    (gntp-notify 'org-gcal title mes "localhost" nil icn)))
(provide 'org-gcal)

;;; org-gcal.el ends here

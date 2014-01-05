;;; org-gcal.el --- Org sync with Google Calendar

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/org-gcal.el
;; Version: 0.1
;; Maintainer: myuhe
;; Copyright (C) :2014 myuhe all rights reserved.
;; Created: :14-01-03
;; Package-Requires: ((request "0.2.0"))
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

(defcustom org-gcal-token-file
  (concat user-emacs-directory ".org-gcal-token")
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

(defconst org-gcal-token-plist nil
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
  (unless org-gcal-token-plist 
    (org-gcal-request-token))
  (cl-loop for x in org-gcal-file-alist
           do
           (lexical-let ((x x)) 
             (deferred:$       
               (request-deferred
                (format org-gcal-events-url (car x))
                :type "GET"        
                :params '((access_token . org-gcal--get-access-token)
                          (key . org-gcal-client-secret)
                          (singleEvents . "True")
                          (timeMin . org-gcal--subsract-time)
                          (timeMax . org-gcal--add-time)
                          ("grant_type" . "authorization_code"))
                :parser 'org-gcal--json-read)
               (deferred:nextc it
                 (lambda (response)
                   (setq temp (request-response-data response))
                   (write-region
                    (mapconcat 'identity
                               (mapcar (lambda (lst) 
                                         (org-gcal--cons-list lst))
                                       (plist-get (request-response-data response) :items ))
                               "") 
                    nil (cdr x))))))))

(defun org-gcal-post-at-point ()
  (interactive)
  (unless org-gcal-token-plist 
    (org-gcal-request-token))
  (save-excursion
    (end-of-line)
    (re-search-backward org-heading-regexp)
    (re-search-forward "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
    (backward-char 11)
    (let* ((elem (org-element-at-point))
           (tobj (org-element-timestamp-parser))
           (smry (replace-regexp-in-string 
                  "\s?<.*$" ""
                  (car (org-element-map elem 'headline (lambda (hl) (org-element-property :raw-value hl))))))
           (loc  (car (org-element-map elem 'headline (lambda (hl) (org-element-property :LOCATION hl)))))
           (id  (car (org-element-map elem 'headline (lambda (hl) (org-element-property :ID hl)))))
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
           (desc (replace-regexp-in-string
                  "\n  :PROPERTIES:\n .:LOCATION:.*\n  :LINK:.*\n  :ID:.*\n  :END:\n" ""
                  (buffer-substring-no-properties
                   (plist-get (cadr elem) :contents-begin)
                   (plist-get (cadr elem) :contents-end)))))
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
   :data '(("client_id" . org-gcal-client-id)
           ("client_secret" . org-gcal-client-secret)
           ("code" . org-gcal-request-authorization)
           ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
           ("grant_type" . "authorization_code"))
   :parser 'org-gcal--json-read
   :success (function* 
             (lambda (&key data &allow-other-keys)
               (when data
                 (setq org-gcal-token-plist data)
                 (with-current-buffer (get-buffer-create " *org-gcal-token*")
                   (erase-buffer)
                   (insert (pp data))
                   (write-region (point-min) (point-max) org-gcal-token-file)))))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                (message "Got error: %S" error-thrown)))))

(defun org-gcal-refresh-token ()
  "Refresh OAuth access at TOKEN-URL."
  (interactive)  
  (request 
   org-gcal-token-url
   :type "POST"        
   :data '(("client_id" . org-gcal-client-id)
           ("client_secret" . org-gcal-client-secret)
           ("refresh_token" . org-gcal--get-refresh-token)
           ("grant_type" . "refresh_token"))
   :parser 'org-gcal--json-read
   :success (function* 
             (lambda (&key data &allow-other-keys)
               (when data
                 (plist-put org-gcal-token-plist
                            ':access_token 
                            (plist-get data ':access_token))
                 (with-current-buffer (get-buffer-create " *org-gcal-token*")
                   (erase-buffer)
                   (insert (pp org-gcal-token-plist))
                   (write-region (point-min) (point-max) org-gcal-token-file)))))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                (message "Got error: %S" error-thrown)))))

;; Internal

(defun org-gcal--json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "^{" nil t)
    (delete-region (point-min) (1- (point)))
    (goto-char (point-min))
    (json-read)))

(defun org-gcal--get-refresh-token ()
  (plist-get org-gcal-token-plist ':refresh_token))

(defun org-gcal--get-access-token ()
  (plist-get org-gcal-token-plist ':access_token))

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
   (if (or hour min) (format "+%02d:00" (/ (car (current-time-zone)) 3600)))))

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
     " " (if (org-gcal--alldayp start end)
             (org-gcal--format-iso2org start)
           (if (and
                (= (plist-get (org-gcal--parse-date start) :year) 
                   (plist-get (org-gcal--parse-date end) :year))
                (= (plist-get (org-gcal--parse-date start) :mon) 
                   (plist-get (org-gcal--parse-date end) :mon))
                (= (plist-get (org-gcal--parse-date start) :day) 
                   (plist-get (org-gcal--parse-date end) :day)))
               (concat "<"
                       (org-gcal--format-date start "%Y-%m-%d %a %H:%M")
                       "-"
                       (org-gcal--format-date end "%H:%M")
                       ">")
             (concat (org-gcal--format-iso2org start)
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
                         ;;(if (< 11 (length str)) "%Y-%m-%d %a %H:%M" "%Y-%m-%d %a")
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
  (unless org-gcal-token-plist 
    (org-gcal-request-token))
  (cl-loop for x in org-gcal-file-alist
           do
           (lexical-let ((stime (org-gcal--param-date start))
                         (etime (org-gcal--param-date end)))
             (request
              (concat
               (format org-gcal-events-url (car x)) 
               (when id 
                 (concat "/" id)))
              :type (if id "PATCH" "POST")
              :headers '(("Content-Type" . "application/json"))
              :data (json-encode `(("start"  (,stime . ,start))  
                                   ("end"  (,etime . ,end))
                                   ("summary" . ,smry)
                                   ("location" . ,loc)
                                   ("description" . ,desc)))
              :params '((access_token . org-gcal--get-access-token)
                        (key . org-gcal-client-secret)
                        ("grant_type" . "authorization_code"))

              :parser 'buffer-string
              :success (function*
                        (lambda (&key data &allow-other-keys)
                          (message "I sent: %S" data)))))))

(provide 'org-gcal)

;;; org-gcal.el ends here

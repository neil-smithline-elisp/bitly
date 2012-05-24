;;;  bitly.el --- Shorten URLs with bit.ly from emacs
;; Copyright (C) 2012 Neil Smithline
;; Copyright (C) 2010 Vivek Haldar
;;
;;   Current version available at:
;;        https://github.com/Neil-Smithline/bitly.el
;;        (or http://bit.ly/wSSiWH)
;;   Original version available at
;;       https://gist.github.com/716717
;;       (or http://bit.ly/wiMWlR)
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; Author: Vivek Haldar <vh@vivekhaldar.com>
;; Created: 27 November 2010
;; Maintainer: Neil Smithline
;;
;;; Commentary: 
;; bitly.el allows shortening URLs through the bit.ly api service. See
;; http://bit.ly/wmT2Sf for info about the bit.ly api service.
;;
;; Installation:
;; You will need to register with bit.ly and get an API key. Customize
;; the variables bitly-username and bitly-api-key.
;;
;; Description:
;; Interactive functions:
;; bitly-shorten
;;     Shorten an URL (prompted for in the minibuffer) and insert at
;;     point.

(require 'url)

(eval-when-compile
  (require 'cl))

(defcustom bitly-username nil
  "Bitly username."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "bitly_username"))
  :group 'bitly)

(defcustom bitly-api-key nil
  "Bitly API key."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "bitly_api_key"))
  :group 'bitly)

(defcustom bitly-shorten-asynchronously t
  "Whether `bitly-shorten' should run asynchronously or not.
Unless you are having probems with `bitly-shorten', there is
probably no need to change this.

If you are calling `bitly-shorten' programmatically, you can set
this to a specific value in a `let' statement."
  :type 'boolean
  :group 'bitly)

(defconst bitly-api-url "http://api.bit.ly/v3/")

(defun bitly-get-me-an-url (prompt)
  "Get an URL, prompting the user if needed.."
  (let ((the-url)
        (begin)
        (end))
    (when (region-active-p)
      (setq the-url (buffer-substring (region-beginning) (region-end))
            begin   (region-beginning)
            end     (region-end)))
    (unless the-url
      (setq the-url (thing-at-point 'url))
      (when the-url
        (setq begin   (beginning-of-thing 'url)
              end     (end-of-thing 'url))))
    (unless the-url
      (setq the-url (read-from-minibuffer prompt)
            begin   (point)
            end     (point)))
    (list the-url begin end)))

(defun bitly-shorten (url &optional begin end)
  "Shorten a full URL using Bitly, and insert at point.
Optional BEGIN and END specify a region to get the URL from and
replace it.

Call Bitly asynchronously if `bitly-shorten-asynchronously'."
  (interactive (bitly-get-me-an-url "URL to shorten wth Bitly: "))
  (assert (and bitly-api-key bitly-username))
  (let* ((api-url (concat bitly-api-url
                          (format
                           "shorten?login=%s&apiKey=%s&longUrl=%s&format=txt"
                           bitly-username
                           bitly-api-key
                           (url-hexify-string url))))
         (start-marker (copy-marker (or begin (point-marker))))
         (end-marker   (copy-marker (or end (point-marker)))))
    (if bitly-shorten-asynchronously
        (url-retrieve api-url
                      #'bitly-process-response
                      (list start-marker end-marker))
      (let ((buf (url-retrieve-synchronously api-url)))
        (with-current-buffer buf
          (bitly-process-response nil start-marker end-marker))))))

(defun bitly-process-response (status start-marker end-marker)
  "Process the Bitly response in the current buffer, with STATUS and REGION.
The shortened URL will be inserted into REGION, a pair of markers."
  (when (eq :error (car status))
    (signal (cadr status) (caddr status)))
  (let ((short-url        (bitly-strip-http-headers (current-buffer))))
    (with-current-buffer (marker-buffer start-marker)
      ;; Save point in an `after' marker per `marker-insertion-type'.
      ;; If point is in the URL this will move point to the end of the
      ;; URL. Otherwise, point stays where it is.
      (let ((orig-point (copy-marker (point-marker) t)))
        (goto-char start-marker)
        (delete-region start-marker end-marker)
        (insert short-url)
        (goto-char orig-point)))))

(defun bitly-strip-http-headers (response-buffer)
  "Destructively strip headers from RESPONSE-BUFFER and return the body."
  (with-current-buffer response-buffer
    (goto-char (point-max))
    ;; Delete terminating newline
    (backward-delete-char 1)
    ;; Delete header and such.
    (beginning-of-line 1)
    (delete-region (point-min) (point))
    (buffer-substring (point-min) (point-max))))

(provide 'bitly)

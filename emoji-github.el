;;; emoji-github.el --- Display list of GitHub's emoji.  (cheat sheet)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-03-05 10:58:18

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Display list of GitHub's emoji.  (cheat sheet)
;; Keyword: list github emoji display handy
;; Version: 0.1.2
;; Package-Requires: ((emacs "24.4") (emojify "1.0") (request "0.3.0"))
;; URL: https://github.com/jcs090218/emoji-github

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display list of GitHub's emoji.  (cheat sheet)
;;

;;; Code:

(require 'tabulated-list)

(require 'emojify)
(require 'request)


(defgroup emoji-github nil
  "Display list of GitHub's emoji.  (cheat sheet)"
  :prefix "emoji-github-"
  :group 'convenience
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/emoji-github"))

(defcustom emoji-github-columns 3
  "Columns to display in each row."
  :type 'integer
  :group 'emoji-github)

(defconst emoji-github--buffer-name "*GitHub Emoji*"
  "Name of the GitHub emoji buffer.")

(defconst emoji-github--api-url "https://api.github.com/emojis"
  "Store GitHub emoji's URL.")

(defvar emoji-github--list '()
  "Store a list of GitHub emoji.")

(defvar emoji-github--column-size 30
  "Character size for each column.")


(defun emoji-github--format ()
  "Return the list format from the display emoji."
  (let ((title "List %s")
        (lst '())
        (cnt 1))
    (while (<= cnt emoji-github-columns)
      (push (list (format title cnt) emoji-github--column-size t) lst)
      (setq cnt (1+ cnt)))
    (setq lst (reverse lst))
    (vconcat lst)))

(defun emoji-github--format-item (item)
  "Return the string form by ITEM (emoji)."
  (if (not (string-empty-p item))
      (cond ((string-match-p "##==--" item) item)  ; For title.
            (t (format ":%s: %s" item item)))      ; For emoji.
    ""))

(defun emoji-github--get-github-emoji ()
  "Get the GitHub emoji by `emoji-github--api-url'."
  (setq emoji-github--list '())  ; Clean before use.
  (message "Getting emoji data from '%s'" emoji-github--api-url)
  (request
   emoji-github--api-url
   :type "GET"
   :parser 'json-read
   :success
   (cl-function
    (lambda (&key data  &allow-other-keys)
      (dolist (item data)
        (push (symbol-name (car item)) emoji-github--list))
      (setq emoji-github--list (reverse emoji-github--list))
      (save-selected-window
        (when (get-buffer emoji-github--buffer-name)
          (with-current-buffer emoji-github--buffer-name
            (setq tabulated-list-entries (emoji-github--get-entries))
            (tabulated-list-revert))))))
   :error
   ;; NOTE: Accept, error.
   (cl-function
    (lambda (&rest args &key _error-thrown &allow-other-keys)
      (user-error "[ERROR] Error while getting GitHub Emoji API.")))))

(defun emoji-github--get-entries ()
  "Get all GitHub's emoji as list entry."
  (let ((entries '()) (index (- (length emoji-github--list) emoji-github-columns)))
    (while (>= index 0)
      (let ((new-entry '()) (new-entry-value '()))
        (let ((col-cnt (1- emoji-github-columns)) (col-val nil))
          (while (>= col-cnt 0)
            (setq col-val (nth (+ index col-cnt) emoji-github--list))
            (push (emoji-github--format-item col-val) new-entry-value)
            (setq col-cnt (1- col-cnt))))
        (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
        (push (number-to-string index) new-entry)
        (push new-entry entries))
      (setq index (- index emoji-github-columns)))
    entries))

(define-derived-mode emoji-github-mode tabulated-list-mode
  "emoji-github-mode"
  "Major mode for displaying GitHub's emoji."
  :group 'emoji-github
  (emoji-github--get-github-emoji)
  (setq tabulated-list-format (emoji-github--format))
  (setq tabulated-list-padding 1)
  (setq-local tabulated-list--header-string (format "URL: %s" emoji-github--api-url))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (emoji-github--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header)
  (emojify-mode 1))

;;;###autoload
(defun emoji-github ()
  "List of GitHub's emoji."
  (interactive)
  (pop-to-buffer emoji-github--buffer-name nil)
  (emoji-github-mode))


(provide 'emoji-github)
;;; emoji-github.el ends here

;;; doc-mode.el --- 
;;
;; Copyright (C) 2007 Nikolaj Schumacher;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 
;; Keywords: 
;; URL: http://nschum.de/src/emacs/doc-mode/
;; Compatibility: GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;;; Changes Log:
;;
;; 2007-??-?? (0.?)
;;    Initial release.
;;
;;; Code:

;; semantic-after-auto-parse-hooks

(defun doc-mode-current-tag ()
  (or (semantic-current-tag-of-class 'function)
      (semantic-current-tag-of-class 'type)))
;;   (let ((overlays (overlays-at (point)))
;;         tag)
;;     (while (and overlays (null tag))
;;       (setq tag (overlay-get (pop overlays) 'doc-mode)))
;;     (or tag (semantic-current-tag-of-class 'function)
;;         (semantic-current-tag-of-class 'type))))

(setq doc-mode-template-begin "/**")
(setq doc-mode-template-end " */")
(setq doc-mode-template-continue " * ")

(setq doc-mode-single-begin "/** ")
(setq doc-mode-single-end " */")

;; (setq doc-mode-overlay-map (make-hash-table :test 'equal))
;; (hash-table-test doc-mode-overlay-map)

;; (maphash (lambda (x y) (message "%s->%s" x y)) doc-mode-overlay-map)

(defun doc-mode-insert (tag column)
  ;; insert new doc
  (let ((lines (doc-mode-format-tag tag))
        (beg (point)))
    (indent-to-column column)
    (if (not (cdr lines))
        (insert doc-mode-single-begin (car lines) doc-mode-single-end "\n")
      (insert doc-mode-template-begin "\n")
      (indent-to-column column)
      (dolist (line lines )
        (insert doc-mode-template-continue line "\n")
        (indent-to-column column))
      (insert doc-mode-template-end "\n"))))

;;; registering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-find-doc-bounds (pos)
  "Find the documentation right before POS.
If there is anything but whitespace between the documentation and POS, nil is
returned.  Otherwise a cons of the doc's beginning and end is given."
  (let (beg end col found)
    (save-excursion
      ;; find the first /** not closed
      (goto-char pos)
      (setq found (search-backward "/**" nil t))
      (while (and found (not (looking-at "\\*/")))
        (setq beg (point))
        (setq found (re-search-backward "/\\*\\*\\|\\*/" nil t)))
      (when beg
        ;; return to last known good position
        (goto-char beg)
        (setq col (current-column))
        ;; move to beginning of line, if whitespace
        (incf beg (skip-chars-backward " \t"))
        ;; search for end
        (when (search-forward "*/" nil t)
          (skip-chars-forward " \t" (point-at-eol))
          (setq end (point))
          ;; check if this is actually right before POS
          (skip-chars-forward " \t\n\r" pos)
          (when (eq pos (point))
            `(:beg ,beg :end ,(1+ end) :column ,col)))))))

;;; formating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-markup (markup &optional description)
  (concat "@" markup (when description " ") description))

(mapcar 'semantic-tag-name (semantic-tag-get-attribute xxx :arguments))

(defun doc-mode-format-tag (tag)
  (append
   `("One line brief documentation.")
   (mapcar (lambda (argument)
             (doc-mode-markup "param" (semantic-tag-name argument)))
           (semantic-tag-get-attribute tag :arguments))
   `(,(doc-mode-markup "return" "The return value"))
   ))
;;   (insert (semantic-tag-name tag)))

;;; extracting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-extract-brief (beg end)
  (buffer-substring-no-properties
   (car (doc-mode-find-summary beg end))
   (cdr (doc-mode-find-summary beg end))))

(defun doc-mode-find-summary (beg end)
  (let ((str (buffer-substring-no-properties beg end)))
    (if (string-match "^[@\\]brief \\([^\r\n]+\n\\)" str)
        (cons (+ (match-beginning 1) beg) (+ (match-end 1) beg))
      (string-match "[^ \t\n\r*/][^\r\n]*\n" str)
      (cons (+ (match-beginning 0) beg) (+ (match-end 0) beg)))))

;;; folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-find-eligible-tags ()
  (let ((tags (semantic-brute-find-tag-by-function
               (lambda (tag) (memq (semantic-tag-class tag) '(type function)))
               (semanticdb-file-stream (buffer-file-name)))))
    (nconc (mapcan 'semantic-tag-components tags) tags)))

(defun doc-mode-fold-tag-comment (tag &optional show)
  (assert doc-mode)
  (let ((hidden (gethash tag doc-mode-overlay-map)))
    ;; always get rid of old hiding overlays
    (mapc 'delete-overlay hidden)
    (if show
        (remhash tag doc-mode-overlay-map)
      (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
        (when bounds
          (let* ((beg (plist-get bounds :beg))
                 (end (plist-get bounds :end))
                 (summary-bounds (doc-mode-find-summary beg end))
                 (before-overlay (make-overlay beg (car summary-bounds)))
                 (after-overlay (make-overlay (cdr summary-bounds) end)))
            (overlay-put before-overlay 'invisible t)
            (overlay-put after-overlay 'invisible t)
            (puthash tag (list before-overlay after-overlay)
                     doc-mode-overlay-map)))))))

(defun doc-mode-fold-all (arg)
  (interactive "P")
  (dolist (tag (doc-mode-find-eligible-tags))
    (doc-mode-fold-tag-comment tag arg)))

;;; keywords ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst doc-mode-keywords
  (eval-when-compile
    `((,(concat "[@\\]" (regexp-opt '("param" "return") t) "\\>")
       (0 font-lock-keyword-face prepend)))))

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun semantic-tag-hash (tag)
  (+ (sxhash (semantic-tag-name tag)) (sxhash (semantic-tag-class tag))))

(define-hash-table-test 'semantic-equivalent-tag-p
  'semantic-equivalent-tag-p 'semantic-tag-hash)

(defvar doc-mode-overlay-map nil)
(make-variable-buffer-local 'doc-mode-overlay-map)

(define-minor-mode doc-mode
  "Minor mode for editing in-code documentation."
  nil " doc" nil
  (if doc-mode
      (progn (font-lock-add-keywords nil doc-mode-keywords)
             (setq doc-mode-overlay-map
                   (make-hash-table :test 'semantic-equivalent-tag-p
                                    :weakness 'value)))
    (doc-mode-fold-all t)
    (font-lock-remove-keywords nil doc-mode-keywords)
    (kill-local-variable 'doc-mode-overlay-map))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-remove (&optional tag)
  "Remove the documentation for TAG, or the appropriate tag at point."
  (interactive)
  (unless tag (setq tag (or (doc-mode-current-tag) (error "No tag found"))))
  ;; try if we find an overlay
  (let ((overlay (gethash tag doc-mode-overlay-map)))
    (if (not (and overlay (overlay-buffer overlay)))
        ;; no overlay, find manually
        (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
          (when bounds
            (delete-region (plist-get bounds :beg) (plist-get bounds :end))))
      (delete-region (overlay-start overlay)
                     (overlay-end overlay))
      (delete-overlay overlay))))

(defun doc-mode-add ()
  (interactive)
  (let ((tag (or (doc-mode-current-tag) (error "No tag found")))
        column)
    (save-excursion
      (goto-char (or (semantic-tag-start tag) (error "No tag found")))
      (doc-mode-remove tag)
      (setq column (current-column))
      (skip-chars-backward " \t" (point-at-bol))
      (doc-mode-insert tag column))
;;       (goto-char (point-at-bol))
      ))

(global-set-key "\C-c\C-d" 'doc-mode-add)
(define-key c-mode-base-map "\C-c\C-d" nil)

(global-set-key "\C-c\C-t" 'doc-mode-fold-all)



(provide 'doc-mode)
;;; doc-mode.el ends here

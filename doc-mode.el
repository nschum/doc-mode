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

;;; insertion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doc-mode-template-begin "/**")
(setq doc-mode-template-end " */")
(setq doc-mode-template-continue " * ")

(setq doc-mode-single-begin "/** ")
(setq doc-mode-single-end " */")

(defun doc-mode-insert (lines indent)
  "Insert a documentation at point.
LINES is a list of strings.  INDENT determines the offset."
  (if (< (current-column) indent)
      (indent-to-column indent)
    (move-to-column indent t))
  (if (not (consp lines))
      (insert doc-mode-single-begin lines doc-mode-single-end ?\n)
    (insert doc-mode-template-begin "\n")
    (indent-to-column indent)
    (dolist (line lines)
      (insert doc-mode-template-continue line "\n")
      (indent-to-column indent))
    (insert doc-mode-template-end ?\n)))

(defun doc-mode-remove-doc (point)
  "Remove the documentation before POINT."
  (let* ((bounds (doc-mode-find-doc-bounds point))
         (beg (plist-get bounds :beg))
         (end (plist-get bounds :end)))
    (when bounds
      (save-excursion
        (goto-char beg)
        (incf beg (skip-chars-backward " \t"))
        (goto-char end)
        (incf end (skip-chars-forward " \t"))
        (when (eolp) (incf end))
        (delete-region beg end)))))

(defun doc-mode-remove-tag-doc (tag)
  "Remove the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (or (doc-mode-current-tag) (error "No tag found"))))
  (doc-mode-remove-doc (semantic-tag-start tag)))

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
;;         (incf beg (skip-chars-backward " \t"))
        ;; search for end
        (when (search-forward "*/" nil t)
;;           (skip-chars-forward " \t" (point-at-eol))
          (setq end (point))
          ;; check if this is actually right before POS
          (skip-chars-forward " \t\n\r" pos)
          (when (eq pos (point))
            `(:beg ,beg :end ,end :column ,col)))))))

;;; formating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-markup (markup &optional argument description)
  (concat "@" markup
          (when argument " ") argument
          (when description " ") description))

(mapcar 'semantic-tag-name (semantic-tag-get-attribute xxx :arguments))

(defun doc-mode-format-tag (tag)
  (append
   `("One line brief documentation.")
   (mapcar (lambda (argument)
             (doc-mode-markup "param" (semantic-tag-name argument)
                              "A value."))
           (semantic-tag-get-attribute tag :arguments))
   `(,(doc-mode-markup "return" "The return value"))
   ))

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

(defun doc-mode-find-eligible-tags ()
  (let ((tags (semantic-brute-find-tag-by-function
               (lambda (tag) (memq (semantic-tag-class tag) '(type function)))
               (semanticdb-file-stream (buffer-file-name)))))
    (nconc (mapcan 'semantic-tag-components tags) tags)))

;;; folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doc-mode-folds nil)
(make-variable-buffer-local 'doc-mode-folds)

(defun doc-mode-fold-doc (point)
  (assert doc-mode)
  (let ((bounds (doc-mode-find-doc-bounds point)))
    (when bounds
      (let* ((beg (plist-get bounds :beg))
             (end (plist-get bounds :end))
             (summary-bounds (doc-mode-find-summary beg end))
             (before-overlay (make-overlay beg (car summary-bounds)))
             (after-overlay (make-overlay (cdr summary-bounds) end))
             (siblings (list before-overlay after-overlay)))
        (dolist (ov siblings)
          (overlay-put ov 'invisible t)
          (overlay-put ov 'isearch-open-invisible-temporary
                       'doc-mode-unfold-by-overlay-temporary)
          (overlay-put ov 'isearch-open-invisible 'doc-mode-unfold-by-overlay)
          (overlay-put ov 'doc-mode-fold siblings))
        (setq doc-mode-folds (append doc-mode-folds siblings))))))

(defun doc-mode-fold-tag-doc (tag)
  "Fold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (or (doc-mode-current-tag) (error "No tag found"))))
  (doc-mode-fold-doc (semantic-tag-start tag)))

(defun doc-mode-unfold-by-overlay (overlay)
  "Unfold OVERLAY and its siblings permanently"
  (dolist (ov (overlay-get overlay 'doc-mode-fold))
    ;; remove overlay
    (setq doc-mode-folds (delq ov doc-mode-folds))
    (delete-overlay ov)
    ;; don't let isearch do anything with it
    (setq isearch-opened-overlays (delq ov isearch-opened-overlays))))

(defun doc-mode-unfold-by-overlay-temporary (overlay invisible)
  "Unfold OVERLAY and its siblings temporarily."
  (dolist (ov (overlay-get overlay 'doc-mode-fold))
    (overlay-put ov 'invisible invisible)))

(defun doc-mode-unfold-doc (point)
  "Unfold the comment before POINT."
  (interactive "d")
  (let ((bounds (doc-mode-find-doc-bounds point)))
    (when bounds
      (let* ((beg (plist-get bounds :beg))
             (end (plist-get bounds :end))
             (overlays (overlays-in beg end))
             anything-done)
        (dolist (ov overlays)
          (when (overlay-get ov 'doc-mode-fold)
            (setq anything-done t)
            (delete-overlay ov)
            (setq doc-mode-folds (delq ov doc-mode-folds))))
        ;; return non-nil, if anything unfolded
        ;; this is used to toggle
        anything-done))))

(defun doc-mode-unfold-tag-doc (tag)
  "Unfold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (or (doc-mode-current-tag) (error "No tag found"))))
  (doc-mode-unfold-doc (semantic-tag-start tag)))

;;; all

(defun doc-mode-fold-all (&optional arg)
  (interactive "P")
  (if arg
      (doc-mode-unfold-all)
    (dolist (tag (doc-mode-find-eligible-tags))
      (doc-mode-fold-tag-comment tag arg))))

(defun doc-mode-unfold-all ()
  (dolist (ov doc-mode-folds)
    (delete-overlay ov))
  (kill-local-variable 'doc-mode-folds))

;;; toggle

(defun doc-mode-toggle-tag-folding (tag)
  "Toggle folding of TAG's documentation.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag)))
  (or (doc-mode-unfold-tag-doc tag)
      (doc-mode-fold-tag-doc tag)))

;;; keywords ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst doc-mode-keywords
  (eval-when-compile
    `((,(concat "[@\\]" (regexp-opt '("return") t) "\\>")
       (0 font-lock-keyword-face prepend))
      (,(concat "\\([@\\]" (regexp-opt '("param"))
                "\\>\\)\\(?:\\s +\\(\\sw\\)+\\)?")
       (1 font-lock-keyword-face prepend)
       (2 font-lock-variable-name-face prepend)
       ))))

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode doc-mode
  "Minor mode for editing in-code documentation."
  nil " doc" nil
  (if doc-mode
      (progn (font-lock-add-keywords nil doc-mode-keywords)
             (setq doc-mode-overlay-map
                   (make-hash-table :test 'semantic-equivalent-tag-p
                                    :weakness 'value)))
    (doc-mode-unfold-all)
    (font-lock-remove-keywords nil doc-mode-keywords))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-add ()
  (interactive)
  (let ((tag (or (doc-mode-current-tag) (error "No tag found")))
        column)
    (save-excursion
      (goto-char (or (semantic-tag-start tag) (error "No tag found")))
      (setq column (current-column))
      (doc-mode-remove tag)
      (skip-chars-backward " \t" (point-at-bol))
      (doc-mode-insert (doc-mode-format-tag tag) column))))

(global-set-key "\C-c\C-d" 'doc-mode-add)
(define-key c-mode-base-map "\C-c\C-d" nil)

(global-set-key "\C-ct" 'doc-mode-fold-all)
(global-set-key "\C-c\C-t" 'doc-mode-toggle-tag-folding)



(provide 'doc-mode)
;;; doc-mode.el ends here

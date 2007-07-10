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

(eval-when-compile (require 'cl))

;; semantic-after-auto-parse-hooks

(defun doc-mode-current-tag ()
  (or (semantic-current-tag-of-class 'function)
      (semantic-current-tag-of-class 'type)))

;;; insertion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doc-mode-template-begin "/**")
(setq doc-mode-template-end " */")
(setq doc-mode-template-continue " * ")

(setq doc-mode-single-begin "/** ")
(setq doc-mode-single-end " */")

;; nil means use comment-fill-column
(setq doc-mode-fill-column nil)

(setq doc-mode-align-descriptions t)

(setq doc-mode-keywords-with-argument-regexp
      (eval-when-compile
        (concat "\\([@\\]" (regexp-opt '("param"))
                "\\>\\)\\(?:\\s +\\(\\sw\\)+\\)?")))

(setq doc-mode-keyword-order '("param" "return"))

(defconst doc-mode-keywords-with-parameter '("param"))

(defun doc-mode-line-indent (line)
  "Determine left side offset when indenting LINE."
  (if (string-match doc-mode-keywords-with-argument-regexp line)
      (1+ (- (match-end 0) (match-beginning 0)))
    (if (string-match "[@\\][^ \t]+" line)
        (match-end 0)
      0)))

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
      (let ((beg (point))
            (fill-column (or doc-mode-fill-column comment-fill-column
                             fill-column))
            (fill-prefix
             (when doc-mode-align-descriptions
               (concat (buffer-substring (point-at-bol) (point))
                       doc-mode-template-continue
                       (make-string (doc-mode-line-indent line) ? )))))
        (insert doc-mode-template-continue line "\n")
        (fill-region beg (point) 'left))
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

(defun doc-mode-format-tag (tag)
  (append
   `("One line brief documentation.")
   (mapcar (lambda (argument)
             (doc-mode-markup "param" (semantic-tag-name argument)
                              "A value."))
           (semantic-tag-get-attribute tag :arguments))
   (unless (equal (semantic-tag-type tag) "void")
     `(,(doc-mode-markup "return" "The return value")))))

(defun doc-mode-format-keyword-list (keywords)
  (let ((summary (unless (consp (car keywords)) (pop keywords)))
        (lines (mapcar (lambda (k)
                         (doc-mode-markup (car k) (cadr k) (car (cddr k))))
                       keywords)))
    (if (and summary
             (string-match "\\`\\([^.]*\\.\\)[ \t]*\\(.*\\)\\'" summary))
        (cons (match-string 1 summary)
              (if (eq (match-beginning 2) (match-end 2))
                  lines
                (cons (match-string 2 summary) lines)))
      lines)))

;;; extracting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-extract-summary (beg end)
  (let ((bounds (doc-mode-find-summary beg end)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun doc-mode-find-summary (beg end)
  (let ((str (buffer-substring-no-properties beg end)))
    (if (string-match "^[@\\]brief \\([^\r\n]+\n\\)" str)
        (cons (+ (match-beginning 1) beg) (+ (match-end 1) beg))
      (string-match "[^ \t\n\r*/][^\r\n]*\n" str)
      (cons (+ (match-beginning 0) beg) (+ (match-end 0) beg)))))

;; TODO: maybe use `c-mask-paragraph'
(defun doc-mode-clean-doc (beg end)
  "Remove the comment delimiters between BEG and END."
  (let (result)
    (save-excursion
      (goto-char beg)
      (when (looking-at "[ \t\n\r]*/\\*")
        (goto-char (match-end 0)))
      (while (re-search-forward "[ \t]*\\*[ \t]*\\(.*\\)*?[ \t]*\\(\\*/\\|\n\\)"
                                end t)
        (setq result (concat result (match-string-no-properties 1) "\n")))
      result)))

(defun doc-mode-extract-keywords (beg end)
  "Extract documentation keywords between BEG and END.
Returns a alist of keywords, where each element is the list (keyword
argument value) or (keyword argument)."
  (let ((str (replace-regexp-in-string "\n" " " 
                                       (doc-mode-clean-doc beg end)))
        pos results keyword parameter)
    (when (setq pos (string-match "[ \t]+\\(.*?\\)[ \t]+[@\\]\\sw" str))
      (push (match-string 1 str) results))
    (while (setq pos (string-match
                      (concat "\\([@\\]\\)\\(.+?\\>\\)\\s +\\(.*?\\)[ \t]*"
                              "\\(\\1\\|\\'\\)") str pos))
      (setq keyword (match-string 2 str))
      (setq parameter (match-string 3 str))
      (setq pos (1- (match-end 0)))
      (if (member keyword doc-mode-keywords-with-parameter)
          (let ((parameter (split-string parameter nil t)))
            (push (list keyword (car parameter)
                        (mapconcat 'identity (cdr parameter) " "))
                  results))
        ;; no argument
        (push (list keyword (match-string 3 str)) results)))
    (nreverse results)))

(defun doc-mode-filter-keyword (keyword keywords)
  (let (results)
    (dolist (k keywords)
      (when (and (consp k) (string= (car k) keyword))
        (push k results)))
    (nreverse results)))

(defun doc-mode-find-eligible-tags ()
  (let ((tags (semantic-brute-find-tag-by-function
               (lambda (tag) (memq (semantic-tag-class tag) '(type function)))
               (semanticdb-file-stream (buffer-file-name)))))
    (nconc (mapcan 'semantic-tag-components tags) tags)))

;;; checking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst doc-mode-position (element list)
  "Return the first position of ELEMENT in LIST.
Returns (length LIST) if no occurrence was found."
  (let ((pos 0))
    (while (and list (not (equal element (pop list))))
      (incf pos))
    pos))

(defun doc-mode-keyword< (a b tag)
  (if (equal (car a) "param")
      (let* ((args (mapcar 'semantic-tag-name
                          (semantic-tag-get-attribute tag :arguments)))
             (a-param (cadr a))
             (b-param (cadr b))
             (a-pos (doc-mode-position a-param args))
             (b-pos (doc-mode-position b-param args)))
        (if (= a-pos b-pos) 
             (string< a-param b-param)
          (< a-pos b-pos)))
    (string< (cadr a) (cadr b))))

(defun doc-mode-sort-keywords (keywords tag)
  (let ((lists (make-vector (1+ (length doc-mode-keyword-order)) nil))
        description)
    ;; skip summary
    (while (not (listp (car keywords))) (push (pop keywords) description))
    (dolist (k keywords)
      (push k (elt lists (doc-mode-position (car k) doc-mode-keyword-order))))
    (let ((i (length lists)) result)
      (while (> i 0)
        (setq result (nconc (sort (elt lists (decf i))
                                  (lambda (a b) (doc-mode-keyword< a b tag)))
                            result)))
      (nconc description result))))

(defun doc-mode-missing-parameters (keywords tag)
  (let ((parameters (mapcar 'cadr (doc-mode-filter-keyword "param" keywords)))
        result)
    (dolist (k (mapcar 'semantic-tag-name
                       (semantic-tag-get-attribute tag :arguments)))
      (unless (member k parameters)
        (push (list "param" k "A value.") result)))
;;         (push (doc-mode-markup "param" k "A value.") result)))
    result))

(defun doc-mode-fix-tag-doc (tag)
  (interactive (list (or (doc-mode-current-tag) (error "No tag found"))))
  (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
    (if (null bounds)
        (doc-mode-add tag)
      (let* ((beg (plist-get bounds :beg))
             (end (plist-get bounds :end))
             (keywords (doc-mode-extract-keywords beg end)))
        (setq keywords (nconc keywords
                              (doc-mode-missing-parameters keywords tag)))
        (doc-mode-remove tag)
        (save-excursion
          (goto-char (semantic-tag-start tag))
          (skip-chars-backward " \t" (point-at-bol))
          (doc-mode-insert
           (doc-mode-format-keyword-list
            (doc-mode-sort-keywords keywords tag))
           (plist-get bounds :column)))))))

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

(defun doc-mode-add (tag)
  (interactive (list (or (doc-mode-current-tag) (error "No tag found"))))
  (save-excursion
    (goto-char (or (semantic-tag-start tag) (error "No tag found")))
    (let ((column (current-column)))
      (doc-mode-remove tag)
      (skip-chars-backward " \t" (point-at-bol))
      (doc-mode-insert (doc-mode-format-tag tag) column))))

(global-set-key "\C-c\C-d" 'doc-mode-add)
(define-key c-mode-base-map "\C-c\C-d" nil)

(global-set-key "\C-ct" 'doc-mode-fold-all)
(global-set-key "\C-c\C-t" 'doc-mode-toggle-tag-folding)



(provide 'doc-mode)
;;; doc-mode.el ends here

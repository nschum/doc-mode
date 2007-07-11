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

(push "^No tag found$" debug-ignored-errors)

;; semantic-after-auto-parse-hooks
;; if parser-state ^, reparse or wait

(defun doc-mode-current-tag ()
  (when (semantic-parse-tree-needs-update-p)
    ;; TODO: should be (bovinate -1), but error in semantic pre4
    (semantic-fetch-tags))
  (save-excursion
    (or (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (progn (skip-chars-forward " \t\n") nil)
        (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (if (not (looking-at "/\\*\\*"))
            (semantic-current-tag-of-class 'type)
          (progn (search-forward "*/" nil t)
                 (skip-chars-forward " \t\n")
                 nil))
        (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (semantic-current-tag-of-class 'type))))

(defun doc-mode-current-tag-or-bust ()
  (or (doc-mode-current-tag) (error "No tag found")))

;;; insertion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doc-mode-template-begin "/**")
(setq doc-mode-template-end " */")
(setq doc-mode-template-continue " * ")

(setq doc-mode-single-begin "/** ")
(setq doc-mode-single-end " */")

(setq doc-mode-allow-single-line-comments t)
(setq doc-mode-fold-single-line-comments t)

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

(defun doc-mode-insert-line (line indent)
  (indent-to-column indent)
  (let ((beg (point)))
    (insert doc-mode-template-continue line)
    (delete-char (- (skip-chars-backward " \t")))
    (when (> (point) (+ beg 2))
      (save-excursion (fill-region beg (point) 'left t)))
    (insert "\n")))

(defun doc-mode-insert (keywords indent)
  "Insert a documentation at point.
LINES is a list of strings.  INDENT determines the offset."
  (if (< (current-column) indent)
      (indent-to-column indent)
    (move-to-column indent t))
  (if (and (not (cdr keywords)) doc-mode-allow-single-line-comments)
      (insert doc-mode-single-begin (car keywords) doc-mode-single-end ?\n)
    (insert doc-mode-template-begin "\n")

    ;; first line
    (when (stringp (car keywords))
      (doc-mode-insert-line (pop keywords) indent))

    (if (cdr keywords)
        (while (stringp (car keywords))
          (doc-mode-insert-line (pop keywords) indent)
          (when (stringp (car keywords))
            (doc-mode-insert-line "" indent)))
      (while (stringp (car keywords))
        (doc-mode-insert-line (pop keywords) indent)))

    (while keywords
      (indent-to-column indent)
      (let* ((line (apply 'doc-mode-markup (pop keywords)))
             (beg (point))
             (fill-column (or doc-mode-fill-column comment-fill-column
                              fill-column))
             (fill-prefix
              (when doc-mode-align-descriptions
                (concat (buffer-substring (point-at-bol) (point))
                        doc-mode-template-continue
                        (make-string (doc-mode-line-indent line) ? )))))
        (insert doc-mode-template-continue line "\n")
        (fill-region beg (point) 'left t)))
    (indent-to-column indent)
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
  (interactive (list (doc-mode-current-tag-or-bust)))
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
        ;; search for end
        (when (search-forward "*/" nil t)
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
  `("<brief>." .
    ,(nconc (mapcar (lambda (argument)
                      (list "param" (semantic-tag-name argument) "<doc>"))
                    (semantic-tag-get-attribute tag :arguments))
            (unless (or (not (eq (semantic-tag-class tag) 'function))
                        (equal (semantic-tag-type tag) "void"))
              '(("return" "<doc>"))))))

;;; extracting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-extract-summary (beg end)
  (let ((bounds (doc-mode-find-summary beg end)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun doc-mode-find-summary (beg end)
  (let ((str (buffer-substring-no-properties beg end)))
    (if (string-match "^[@\\]brief \\([^\r\n]+\n\\)" str)
        (cons (+ (match-beginning 1) beg) (+ (match-end 1) beg))
      (string-match "\\([^ \t\n\r*/][^\r\n]*?\\)[ \t]*\\(\n\\|\\*/\\)" str)
      (cons (+ (match-beginning 1) beg) (+ (match-end 1) beg)))))

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

;; (string-match "aa\nbb" "aa\nbb")
(split-string "aaa\nbbb\n\n\nccc\nddd@e" "\n")

(defun doc-mode-extract-keywords (beg end)
  "Extract documentation keywords between BEG and END.
Returns a alist of keywords, where each element is the list (keyword
argument value) or (keyword argument)."
  (let* ((doc (doc-mode-clean-doc beg end))
         (paragraphs (if (string-match "\\(\\(.\\|\n\\)*?\\)[@\\]\\<" doc)
                         (match-string 1 doc) doc))
         match pos results)
    ;; first line summary
    (when (string-match "\\`[ \t\n]*\\(.+\\.\\)[ \n]" paragraphs)
      (push (match-string 1 paragraphs) results)
      (setq pos (match-end 0)))

    ;; other paragraphs
    (dolist (paragraph (split-string (substring paragraphs pos)
                                     "[ \t]*\n\\(\n+[ \t]*\\|$\\)" t))
      (push (replace-regexp-in-string "[\n\r]" " " paragraph) results))

    (setq doc (replace-regexp-in-string "[\n\r]" " " doc))

    (while (string-match
            (concat "\\([@\\]\\)\\(.+?\\>\\)\\s +\\(.*?\\)[ \t\n]*"
                    "\\(\\1\\|\\'\\)") doc pos)
      (match-string 0 doc)
      (setq keyword (match-string 2 doc))
      (setq parameter (match-string 3 doc))
      (setq pos (1- (match-end 0)))
      (if (member keyword doc-mode-keywords-with-parameter)
          (let ((parameter (split-string parameter nil t)))
            (push (list keyword (car parameter)
                        (mapconcat 'identity (cdr parameter) " "))
                  results))
        ;; no argument
        (push (list keyword (match-string 3 doc)) results))
      )
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
    ;; skip text
    (while (stringp (car keywords)) (push (pop keywords) description))
    (dolist (k keywords)
      (push k (elt lists (doc-mode-position (car k) doc-mode-keyword-order))))
    (let ((i (length lists)) result)
      (while (> i 0)
        (setq result (nconc (sort (elt lists (decf i))
                                  (lambda (a b) (doc-mode-keyword< a b tag)))
                            result)))
      (nconc (nreverse description) result))))

(defun doc-mode-missing-parameters (keywords tag)
  (let ((parameters (mapcar 'cadr (doc-mode-filter-keyword "param" keywords)))
        result)
    (dolist (k (mapcar 'semantic-tag-name
                       (semantic-tag-get-attribute tag :arguments)))
      (unless (member k parameters)
        (push (list "param" k "A value.") result)))
    result))

(defun doc-mode-invalid-parameters (keywords tag)
  (let ((parameters (mapcar 'semantic-tag-name
                            (semantic-tag-get-attribute tag :arguments)))
        result)
    (dolist (k (doc-mode-filter-keyword "param" keywords))
      (unless (member (cadr k) parameters)
        (push k result)))
    result))

(defun doc-mode-fix-tag-doc (tag)
  (interactive (list (doc-mode-current-tag-or-bust)))
  (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
    (if (null bounds)
        (doc-mode-add tag)
      (let* ((beg (plist-get bounds :beg))
             (end (plist-get bounds :end))
             (keywords (doc-mode-extract-keywords beg end))
             (missing (doc-mode-missing-parameters keywords tag))
             (invalid (doc-mode-invalid-parameters keywords tag)))
        (if (= (length missing) (length invalid))
            (while missing
              (setcar (cdr (pop invalid)) (cadr (pop missing))))
          (setq keywords (nconc keywords missing)))
        (doc-mode-remove tag)
        (save-excursion
          (goto-char (semantic-tag-start tag))
          (skip-chars-backward " \t" (point-at-bol))
          (doc-mode-insert (doc-mode-sort-keywords keywords tag)
                           (plist-get bounds :column)))))))

(defun doc-mode-format-message (type &optional parameters)
  (if (eq type 'none)
      "No documentation:"
    (when parameters
      (concat (case type
                ('missing "Missing")
                ('invalid "Invalid"))
              " parameter" (when (cdr parameters) "s") ": "
              (mapconcat 'identity (mapcar 'cadr parameters) ", ")))))


(defun doc-mode-check-tag-doc (tag)
  (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
    (if bounds
        (let* ((beg (plist-get bounds :beg))
               (end (plist-get bounds :end))
               (keywords (doc-mode-extract-keywords beg end))
               (missing (doc-mode-format-message 'missing
                                                 (doc-mode-missing-parameters
                                                  keywords tag)))
               (invalid (doc-mode-format-message 'invalid
                                                 (doc-mode-invalid-parameters
                                                  keywords tag))))
          (or (and invalid missing
                   (concat invalid "\n" missing))
              invalid
              missing))
      (doc-mode-format-message 'none))))

(defun doc-mode-first-faulty-tag-doc ()
  (let ((tags (doc-mode-find-eligible-tags)) invalid)
    (while tags
      (if (setq invalid (doc-mode-check-tag-doc (car tags)))
          (setq invalid (cons (car tags) invalid) tags nil)
        (pop tags)))
    invalid))

(defun doc-mode-check-buffer ()
  (interactive)
  (let ((invalid-p (doc-mode-first-faulty-tag-doc)))
    (setq doc-mode-lighter (if invalid-p " doc!" " doc"))
    invalid-p))

(defun doc-mode-next-faulty-doc ()
  "Jump to the next faulty documentation and print error."
  (interactive)
  (let ((tag (doc-mode-first-faulty-tag-doc)))
    (if (null tag)
        (message "No faulty doc found")
      (goto-char (semantic-tag-start (car tag)))
      (message "%s" (cdr tag)))))

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
        (when (or doc-mode-fold-single-line-comments
                  (> (count-lines beg end) 1))
          (dolist (ov siblings)
            (overlay-put ov 'invisible t)
            (overlay-put ov 'isearch-open-invisible-temporary
                         'doc-mode-unfold-by-overlay-temporary)
            (overlay-put ov 'isearch-open-invisible 'doc-mode-unfold-by-overlay)
            (overlay-put ov 'doc-mode-fold siblings))
          (setq doc-mode-folds (nconc doc-mode-folds siblings)))))))

(defun doc-mode-fold-tag-doc (tag)
  "Fold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
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
  (interactive (list (doc-mode-current-tag-or-bust)))
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
  (interactive (list (doc-mode-current-tag-or-bust)))
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

(defvar doc-mode-lighter " doc")

(define-minor-mode doc-mode
  "Minor mode for editing in-code documentation."
  nil doc-mode-lighter nil
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
  (interactive (list (doc-mode-current-tag-or-bust)))
  (save-excursion
    (goto-char (or (semantic-tag-start tag) (error "No tag found")))
    (let ((column (current-column)))
      (doc-mode-remove tag)
      (skip-chars-backward " \t" (point-at-bol))
      (doc-mode-insert (doc-mode-format-tag tag) column))))

(global-set-key "\C-c\C-d" 'doc-mode-fix-tag-doc)
(define-key c-mode-base-map "\C-c\C-d" nil)

(global-set-key "\C-ct" 'doc-mode-fold-all)
(global-set-key "\C-c\C-t" 'doc-mode-toggle-tag-folding)



(provide 'doc-mode)
;;; doc-mode.el ends here

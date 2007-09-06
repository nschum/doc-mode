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
(require 'semantic)
(require 'cc-mode)
(require 'newcomment) ;comment-fill-column

(setq debug-ignored-errors `("^No tag found$"
                             "^Semantic can't parse buffer$"
                             "^No template found$"
                             "^doc-mode not enabled$"
                             "^Beginning of buffer$"
                             "^End of buffer$"
                             . ,debug-ignored-errors))

;; semantic-after-auto-parse-hooks

(defgroup doc-mode nil
  "Minor mode for editing in-code documentation."
  :group 'convenience
  :group 'tools)

(defcustom doc-mode-prefix-key "\C-cd"
  "*Prefix key to use for `doc-mode'.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-auto-check-p t
  "*Should the buffer documentation be checked after a Semantic reparse."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom doc-mode-jump-to-template t
  "*Should the point be moved inside the template after inserting a doc."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defun doc-mode-current-tag ()
  (when (semantic-parse-tree-unparseable-p)
    (error "Semantic can't parse buffer"))
  (when (or (semantic-parse-tree-needs-rebuild-p)
            (semantic-parse-tree-needs-update-p))
    (condition-case nil
        (semantic-fetch-tags)
      (error (error "Semantic can't parse buffer"))))
  (save-excursion
    (or (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (progn (beginning-of-line) (skip-chars-forward " \t\n") nil)
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

;;; templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doc-mode-templates nil)
(make-variable-buffer-local 'doc-mode-templates)

(defun doc-mode-add-template (beg end)
  (let ((overlay (make-overlay beg (point))))
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'face 'highlight)
    (overlay-put overlay 'insert-in-front-hooks '(doc-mode-replace-overlay))
    (overlay-put overlay 'modification-hooks '(doc-mode-delete-overlay))
    (push overlay doc-mode-templates)))

(defvar doc-mode-temp nil)

(defun doc-mode-delete-overlay (ov after-p beg end &optional r)
  (unless after-p
    (mapc 'doc-mode-unfold-by-overlay
          (overlays-in (1- (overlay-start ov)) (1+ (overlay-end ov))))
    (delete-overlay ov)
    (setq doc-mode-templates (delq ov doc-mode-templates))))

(defun doc-mode-replace-overlay (ov after-p beg end &optional r)
  (unless after-p
    (let ((inhibit-modification-hooks nil))
      (delete-region (overlay-start ov) (overlay-end ov)))))

;;;###autoload
(defun doc-mode-next-template ()
  "Jump to the next unfinished documentation template."
  (interactive)
  (let ((min-start (point-max))
        (pos (point))
        start)
    (dolist (ov doc-mode-templates)
      (setq start (overlay-start ov))
      (and (> start pos)
           (< start min-start)
           (setq min-start start)))
    (when (= min-start (point-max))
      (error "End of buffer"))
    (push-mark)
    (goto-char min-start)))

;;;###autoload
(defun doc-mode-previous-template ()
  "Jump to the previous unfinished documentation template."
  (interactive)
  (let ((max-start (point-min))
        (pos (point))
        start)
    (dolist (ov doc-mode-templates)
      (setq start (overlay-start ov))
      (and (< start pos)
           (> start max-start)
           (setq max-start start)))
    (when (= max-start (point-min))
      (error "Beginning of buffer"))
    (push-mark)
    (goto-char max-start)))

;;;###autoload
(defun doc-mode-first-template ()
  "Jump to the oldest unfinished documentation template."
  (interactive)
  (unless doc-mode-templates
    (error "No template found"))
  (push-mark)
  (goto-char (overlay-start (car (last doc-mode-templates)))))

;;; insertion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doc-mode-template-begin "/**")
(setq doc-mode-template-end " */")
(setq doc-mode-template-continue " * ")

(setq doc-mode-keyword-anchor "@")

(setq doc-mode-single-begin "/** ")
(setq doc-mode-single-end " */")

(setq doc-mode-allow-single-line-comments t)
(setq doc-mode-fold-single-line-comments t)

;; nil means use comment-fill-column
(setq doc-mode-fill-column nil)

;; numbers allowed
(setq doc-mode-align-descriptions t)

(setq doc-mode-keywords-with-argument-regexp
      (eval-when-compile
        (concat "\\([@\\]" (regexp-opt '("param"))
                "\\>\\)\\(?:\\s +\\(\\sw\\)+\\)?")))

(setq doc-mode-keyword-order '("param" "return"))

(defconst doc-mode-keywords-with-parameter '("param"))

(defun doc-mode-line-indent (keyword)
  "Determine left side offset when indenting LINE."
  (if (numberp doc-mode-align-descriptions)
      doc-mode-align-descriptions
    (+ 1 (length (car keyword))
       (if (member (car keyword) doc-mode-keywords-with-parameter)
           (1+ (length (cdr keyword)))
         0))))

(defun doc-mode-insert (text)
  "Insert TEXT if a string, or a template if 'prompt."
  (if (stringp text)
      (insert text)
    (let ((beg (point)))
      (insert (cadr text))
      (when doc-mode
        (doc-mode-add-template beg (point))))))

(defun doc-mode-insert-markup (markup &optional argument description)
  (insert doc-mode-keyword-anchor markup)
  (when argument
    (insert " ")
    (doc-mode-insert argument))
  (when description
    (insert " ")
    (doc-mode-insert description)))

(defun doc-mode-insert-line (line indent)
  (indent-to-column indent)
  (let ((beg (point)))
    (insert doc-mode-template-continue)
    (if (and (consp line) (not (eq (car line) 'prompt)))
        (apply 'doc-mode-insert-markup line)
      (doc-mode-insert line))
    (delete-char (- (skip-chars-backward " \t")))
    (when (> (point) (+ beg 2))
      (save-excursion (fill-region beg (point) 'left t)))
    (insert "\n")))

(defun doc-mode-insert-keyword (keyword indent)
  (indent-to-column indent)
  (let* (;;(line (apply 'doc-mode-markup keyword))
         (beg (point))
         (fill-column (or doc-mode-fill-column comment-fill-column fill-column))
         (fill-prefix (when doc-mode-align-descriptions
                        (concat (buffer-substring (point-at-bol) (point))
                                doc-mode-template-continue
                                (make-string (doc-mode-line-indent keyword) ? )
                                ))))
    (doc-mode-insert-line keyword indent)))

(defun doc-mode-insert-doc (keywords &optional pos)
  "Insert a documentation at POS.
LINES is a list of keywords."
  (save-excursion
    (when pos (goto-char pos))
    (let ((indent (current-column)))

      (if (and (not (cdr keywords)) doc-mode-allow-single-line-comments)
          (progn (insert doc-mode-single-begin)
                 (doc-mode-insert (car keywords))
                 (insert doc-mode-single-end "\n"))
        (insert doc-mode-template-begin "\n")

        ;; first line
        (when (or (stringp (car keywords))
                  (eq 'prompt (caar keywords)))
          (doc-mode-insert-line (pop keywords) indent))

        ;; paragraphs
        (if (cdr keywords)
            (while (stringp (car keywords))
              (doc-mode-insert-line (pop keywords) indent)
              (when (stringp (car keywords))
                (doc-mode-insert-line "" indent)))
          (while (stringp (car keywords))
            (doc-mode-insert-line (pop keywords) indent)))

        ;; keywords
        (while keywords
          (doc-mode-insert-keyword (pop keywords) indent))
        (indent-to-column indent)
        (insert doc-mode-template-end "\n"))

      ;; re-indent original line
      (if (< (current-column) indent)
          (indent-to-column indent)
        (move-to-column indent t))))

    (and doc-mode-jump-to-template doc-mode-templates
         (doc-mode-first-template)))

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

;;;###autoload
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
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t\n")
    (let ((end (point)))
      (cond
       ;; /// Doxygen comment */
       ((looking-back "[ \t]*///\\(.*\\)$")
        (forward-line -1)
        (while (looking-at "[ \t]*///\\(.*\\)$")
          (forward-line -1))
        (forward-line 1)
        (skip-chars-forward " \t")
        `(:beg ,(point) :end ,end :column ,(current-indentation)))
       ;; /** JavaDoc comment */
       ((looking-back "\\*/")
        (goto-char (match-beginning 0))
        ;; search for /**, not allowing any */ in between
        (when (and (re-search-backward "\\(/\\*\\*\\|\\*/\\)" nil t)
                   (match-beginning 1))
          `(:beg ,(point) :end ,end :column ,(current-column))))))))

;;; formating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-markup (markup &optional argument description)
  (concat "@" markup
          (when argument " ") argument
          (when description " ") description))

(defun doc-mode-new-keyword (keyword &optional argument)
  (if (member keyword doc-mode-keywords-with-parameter)
      (list keyword argument '(prompt "<doc>"))
    (list keyword '(prompt "<doc>"))))

(defun doc-mode-format-tag (tag)
  (cons `(prompt ,(format "Description for %s." (semantic-tag-name tag)))
        (nconc (mapcar (lambda (argument)
                         (doc-mode-new-keyword "param"
                                               (semantic-tag-name argument)))
                       (semantic-tag-get-attribute tag :arguments))
               (and (eq (semantic-tag-class tag) 'function)
                    (not (equal (semantic-tag-type tag) "void"))
                    (not (semantic-tag-get-attribute tag :prototype-flag))
                    (list (doc-mode-new-keyword "return"))))))

;;; extracting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doc-mode-extract-summary (beg end)
  (let ((bounds (doc-mode-find-summary beg end)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun doc-mode-find-summary (beg end)
  (let ((str (buffer-substring-no-properties beg end)))
    (if (string-match "^[@\\]brief \\([^\r\n]+\n\\)" str)
        (cons (+ (match-beginning 1) beg) (+ (match-end 1) beg))
      (save-excursion
        (goto-char beg)
        (cond
         ;; /// Doxygen comment */
         ((looking-at "///[ \t]*\\(.*\\)[ \t]*$")
          (cons (match-beginning 1) (match-end 1)))
         ;; /** JavaDoc comment */
         ((looking-at "/\\*\\*")
          (goto-char (match-end 0))
          (skip-chars-forward " \t\n*")
          (when (looking-at "\\(.*?\\)[ \t]*\\($\\|\\*+/\\)")
            (cons (match-beginning 1) (match-end 1))))
         (t (cons beg end)))))))

(defun doc-mode-clean-doc (beg end)
  "Remove the comment delimiters between BEG and END."
  (save-excursion
    (goto-char beg)
    (when (looking-at "[ \t\n\r]*/\\*\\*+")
      (setq beg (match-end 0)))
    (goto-char end)
    (when (looking-back "[ \t\n\r]\\*+/")
      (setq end (match-beginning 0)))
    (mapconcat 'identity
               (split-string (buffer-substring-no-properties beg end)
                             "[ \t]*\n[ \t]*\\*/?[ \t]*")
               "\n")))

(defun doc-mode-extract-keywords (beg end)
  "Extract documentation keywords between BEG and END.
Returns a alist of keywords, where each element is the list (keyword
argument value) or (keyword argument)."
  (let* ((paragraphs (doc-mode-clean-doc beg end))
         (doc "")
         (pos 0)
         match results)

    (when (string-match
           "[ \t\n]*\\(\\(.\\|\n\\)*?\\)\\([@\\]\\<\\(.\\|\n\\)*\\'\\)"
           paragraphs)
      (setq doc (match-string-no-properties 3 paragraphs)
            paragraphs (match-string-no-properties 1 paragraphs)))

    ;; first line summary
    (when (string-match "\\`[ \t\n]*\\(.+\\.\\)\\([ \n]\\|\\'\\)" paragraphs)
      (push (match-string 1 paragraphs) results)
      (setq pos (match-end 0)))

    ;; other paragraphs
    (dolist (paragraph (split-string (substring paragraphs pos)
                                     "[ \t]*\n\\(\n+[ \t]*\\|$\\)" t))
      (push (replace-regexp-in-string "[\n\r]" " " paragraph) results))

    ;; keywords
    (dolist (keyword (cdr (split-string doc "[@\\]\\<")))
      (setq match (split-string keyword))
      (push (if (member (car match) doc-mode-keywords-with-parameter)
                (list (car match) (cadr match)
                      (mapconcat 'identity (cddr match) " "))
              (list (car match) (mapconcat 'identity (cdr match) " ")))
            results))
    (nreverse results)))

(defun doc-mode-find-keyword (keyword keywords)
  (let (results)
    (dolist (k keywords)
      (when (and (consp k) (string= (car k) keyword))
        (push k results)))
    (nreverse results)))

(defun doc-mode-filter-keyword (keyword keywords)
  (let (results)
    (dolist (k keywords)
      (unless (and (consp k) (string= (car k) keyword))
        (push k results)))
    (nreverse results)))

(defun doc-mode-find-eligible-tags ()
  (when buffer-file-name
    (unless (or (semantic-parse-tree-unparseable-p)
                (semantic-parse-tree-needs-rebuild-p)
                (semantic-parse-tree-needs-update-p))
      (ignore-errors
        (let (tags)
          (semantic-brute-find-tag-by-function
           (lambda (tag)
             (when (semantic-tag-start tag)
               (case (semantic-tag-class tag)
                 ((function variable) (push tag tags))
                 (type (setq tags
                             (nconc (semantic-tag-type-members tag)
                                    tags))))))
           (semanticdb-file-stream buffer-file-name))
          tags)))))

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
    (dolist (k keywords)
      (if (stringp k)
          (push k description)
        (push k (elt lists (doc-mode-position (car k)
                                              doc-mode-keyword-order)))))
    (let ((i (length lists)) result)
      (while (> i 0)
        (setq result (nconc (sort (elt lists (decf i))
                                  (lambda (a b) (doc-mode-keyword< a b tag)))
                            result)))
      (nconc (nreverse description) result))))

(defun doc-mode-missing-parameters (keywords tag)
  (let ((parameters (mapcar 'cadr (doc-mode-find-keyword "param" keywords)))
        result)
    (dolist (k (mapcar 'semantic-tag-name
                       (semantic-tag-get-attribute tag :arguments)))
      (unless (member k parameters)
        (push (doc-mode-new-keyword "param" k) result)))
    result))

(defun doc-mode-invalid-parameters (keywords tag)
  (let ((parameters (mapcar 'semantic-tag-name
                            (semantic-tag-get-attribute tag :arguments)))
        result)
    (dolist (k (doc-mode-find-keyword "param" keywords))
      (unless (member (cadr k) parameters)
        (push k result)))
    result))

;;;###autoload
(defun doc-mode-fix-tag-doc (tag)
  (interactive (list (doc-mode-current-tag-or-bust)))
  (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
    (if (null bounds)
        (doc-mode-add-tag-doc tag)
      (let* ((beg (plist-get bounds :beg))
             (end (plist-get bounds :end))
             (keywords (doc-mode-extract-keywords beg end))
             (missing (doc-mode-missing-parameters keywords tag))
             (invalid (doc-mode-invalid-parameters keywords tag)))
        ;; fix parameters
        (if (= (length missing) (length invalid))
            (while missing
              (setcar (cdr (pop invalid)) (cadr (pop missing))))
          (setq keywords (nconc keywords missing)))
        ;; fix return value
        (when (eq (semantic-tag-class tag) 'function)
          (if (or (equal (semantic-tag-type tag) "void")
                  (semantic-tag-get-attribute tag :prototype-flag))
              ;; remove
              (setq keywords (doc-mode-filter-keyword "return" keywords))
            ;; add
            (unless (doc-mode-find-keyword "return" keywords)
              (push (doc-mode-new-keyword "return") keywords))))

        (doc-mode-remove-tag-doc tag)
        (doc-mode-insert-doc (doc-mode-sort-keywords keywords tag)
                             (semantic-tag-start tag))))))


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

;;;###autoload
(defun doc-mode-check-buffer ()
  (interactive)
  (let ((invalid-p (doc-mode-first-faulty-tag-doc)))
    (setq doc-mode-lighter (if invalid-p " doc!" " doc"))
    invalid-p))

;;;###autoload
(defun doc-mode-next-faulty-doc ()
  "Jump to the next faulty documentation and print error."
  (interactive)
  (let ((tag (doc-mode-first-faulty-tag-doc)))
    (if (null tag)
        (message "Documentation checked")
      (push-mark)
      (goto-char (semantic-tag-start (car tag)))
      (message "%s" (cdr tag)))))

;;; folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doc-mode-folds nil)
(make-variable-buffer-local 'doc-mode-folds)

(defun doc-mode-fold-doc (point)
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

;;;###autoload
(defun doc-mode-fold-tag-doc (tag)
  "Fold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (unless doc-mode
    (error "doc-mode not enabled"))
  (doc-mode-fold-doc (semantic-tag-start tag)))

(defun doc-mode-unfold-by-overlay (overlay &rest foo)
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

;;;###autoload
(defun doc-mode-unfold-doc (point)
  "Unfold the comment before POINT."
  (interactive "d")
  (unless doc-mode
    (error "doc-mode not enabled"))
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

;;;###autoload
(defun doc-mode-unfold-tag-doc (tag)
  "Unfold the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (unless doc-mode
    (error "doc-mode not enabled"))
  (doc-mode-unfold-doc (semantic-tag-start tag)))

;;; all

;;;###autoload
(defun doc-mode-fold-all (&optional arg)
  (interactive "P")
  (unless doc-mode
    (error "doc-mode not enabled"))
  (if arg
      (doc-mode-unfold-all)
    (dolist (tag (doc-mode-find-eligible-tags))
      (doc-mode-fold-tag-doc tag))))

;;;###autoload
(defun doc-mode-unfold-all ()
  (interactive)
  (dolist (ov doc-mode-folds)
    (delete-overlay ov))
  (kill-local-variable 'doc-mode-folds))

;;; toggle

;;;###autoload
(defun doc-mode-toggle-tag-doc-folding (tag)
  "Toggle folding of TAG's documentation.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (or (doc-mode-unfold-tag-doc tag)
      (doc-mode-fold-tag-doc tag)))

;;; keywords ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst doc-mode-font-lock-keywords
  (eval-when-compile
    `((,(concat "[@\\]" (regexp-opt '("return") t) "\\>")
       (0 font-lock-keyword-face prepend))
      (,(concat "\\([@\\]" (regexp-opt '("param") t)
                "\\>\\)\\(?:[ \t]+\\(\\sw+\\)\\)?")
       (1 font-lock-keyword-face prepend)
       (3 font-lock-variable-name-face prepend)
       ))))

;;; mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doc-mode-lighter " doc")

(defvar doc-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'doc-mode-fix-tag-doc)
    (define-key map "c" 'doc-mode-check-tag-doc)
    (define-key map "t" 'doc-mode-toggle-tag-doc-folding)
    (define-key map "f" 'doc-mode-fold-tag-doc)
    (define-key map "u" 'doc-mode-unfold-tag-doc)
    (define-key map "r" 'doc-mode-remove-tag-doc)
    (define-key map "i" 'doc-mode-add-tag-doc)
    (define-key map "e" 'doc-mode-next-faulty-doc)
    (define-key map "n" 'doc-mode-next-template)
    (define-key map "\C-c" 'doc-mode-check-buffer)
    (define-key map "\C-f" 'doc-mode-fold-all)
    (define-key map "\C-u" 'doc-mode-unfold-all)
    map))

;;;###autoload
(define-minor-mode doc-mode
  "Minor mode for editing in-code documentation."
  nil doc-mode-lighter (list (cons doc-mode-prefix-key doc-mode-prefix-map))
  (if doc-mode
      (progn
        (font-lock-add-keywords nil doc-mode-font-lock-keywords)
        (when doc-mode-auto-check-p
          (add-hook 'semantic-after-auto-parse-hooks 'doc-mode-check-buffer
                    nil t)
          (add-hook 'semantic-after-idle-scheduler-reparse-hooks
                    'doc-mode-check-buffer nil t)))
    (dolist (ov doc-mode-templates)
      (delete-overlay ov))
    (kill-local-variable 'doc-mode-templates)
    (doc-mode-unfold-all)
    (font-lock-remove-keywords nil doc-mode-font-lock-keywords)
    (remove-hook 'semantic-after-auto-parse-hooks 'doc-mode-check-buffer t)
    (remove-hook 'semantic-after-idle-scheduler-reparse-hooks
                 'doc-mode-check-buffer t))

  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun doc-mode-add-tag-doc (tag)
  (interactive (list (doc-mode-current-tag-or-bust)))
  (doc-mode-insert-doc (doc-mode-format-tag tag) (semantic-tag-start tag)))

(provide 'doc-mode)

;;; doc-mode.el ends here

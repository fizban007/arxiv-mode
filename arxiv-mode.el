;;; arxiv-mode.el --- Read and search for articles on arXiv.org  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021 Alex Chen, Simon Lin

;; Author: Alex Chen (fizban007) <fizban007@gmail.com>
;;         Simon Lin (Simon-Lin) <n.sibetz@gmail.com>
;; URL: https://github.com/fizban007/arxiv-mode
;; Version: 0.3.1
;; Keywords: bib, convenience, hypermedia
;; Package-Requires: ((emacs "27.1") (hydra "0"))
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; arxiv-mode is an Emacs major mode for viewing
;; updates on arXiv.org.
;;
;;
;; Common Usage
;; ============
;;
;; arxiv-mode provides many functions for accessing arXiv.org.
;; To browse the daily new submissions list in a category, run `M-x arxiv-read-new`.
;; To browse the recent (weekly) submissions, run `M-x arxiv-read-recent`.
;; Use `M-x arxiv-read-author` to search for specific author(s).
;; Use `M-x arxiv-search` to perform a simple search on the arXiv database.
;;
;; For more complicated searches, use `M-x arxiv-complex-search`.
;; This command allows user to dynamically refine and modify search conditions.
;; You can also use `r` to refine search condition in the abstract list obtained from a search.
;;
;; In the article list, use `n` and `p` to navigate the article list.
;; Press `SPC` to toggle visibility of the abstract window.
;; Press `RET` to open the entry in a web browser. Press `d` to download the pdf.
;; Press `b` to export the bibtex entry of current paper to your specified .bib file.
;; Press `B` to export the bibtex entry to a new buffer.
;; Press `e` to download pdf and add a bibtex entry with a link to the actual pdf file.
;;
;; All available commands are listed in a hydra help menu accessable by `?` whenever you are in the article list.
;;
;;
;; Installation
;; ============
;;
;; arxiv-mode is available on MELPA.
;; After `M-x package-install RET arxiv-mode RET`, put the following code in your `init.el`:
;;
;; (require 'arxiv-mode)
;;
;; Or if you use `use-package`, you can simply put:
;; (use-package arxiv-mode
;;   :ensure t)
;;
;; into the your init file. `use-package` will automatically download `arxiv-mode` for you.
;;
;;
;; Customization
;; =============
;;
;; Run `M-x arxiv-customize` to customize or set the customization variables directly.




;;; Code:

(require 'seq)
(require 'button)
(require 'hydra)
(require 'bibtex)
(require 'arxiv-vars)
(require 'arxiv-query)

(defvar arxiv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'arxiv-prev-entry)
    (define-key map (kbd "n") 'arxiv-next-entry)
    (define-key map (kbd "RET") 'arxiv-open-current-url)
    (define-key map (kbd "SPC") 'arxiv-SPC)
    (define-key map (kbd "d") 'arxiv-download-pdf)
    (define-key map (kbd "e") 'arxiv-download-pdf-export-bibtex)
    (define-key map (kbd "b") 'arxiv-export-bibtex)
    (define-key map (kbd "B") 'arxiv-export-bibtex-to-buffer)
    (define-key map (kbd "r") 'arxiv-refine-search)
    (define-key map (kbd "q") 'arxiv-exit)
    (define-key map (kbd "?") 'arxiv-help-menu/body)
    (define-key map (kbd "<mouse-1>") 'arxiv-click-select-entry)
    map))

(define-derived-mode arxiv-mode special-mode "arXiv"
  "Major mode for reading updates and searching on arXiv.org.
Type SPC to expand details on selected entry.
Type RET to visit the corresponding entry on arXiv.org in browser.
Type ? to invoke major commands."
  :group 'arxiv
  (setq header-line-format '(:eval (arxiv-headerline-format)))
  (setq arxiv-highlight-overlay (make-overlay 1 1))
  (overlay-put arxiv-highlight-overlay 'face '(:inherit highlight :extend t))
  (if arxiv-use-variable-pitch
      (variable-pitch-mode 1)
    (variable-pitch-mode -1)))

(defvar arxiv-abstract-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'arxiv-open-current-url)
    (define-key map (kbd "SPC") 'arxiv-toggle-abstract)
    (define-key map (kbd "d") 'arxiv-download-pdf)
    (define-key map (kbd "e") 'arxiv-download-pdf-export-bibtex)
    (define-key map (kbd "b") 'arxiv-export-bibtex)
    (define-key map (kbd "B") 'arxiv-export-bibtex-to-buffer)
    (define-key map (kbd "q") 'arxiv-exit)
    map))

(define-derived-mode arxiv-abstract-mode special-mode "arXiv-abstract"
  "Major mode for reading arXiv abstracts."
  (if arxiv-use-variable-pitch
      (variable-pitch-mode 1)
    (variable-pitch-mode -1)))

(defun arxiv-insert-with-face (string face-property)
  "Wrapper function to insert STRING with given FACE-PROPERTY."
  (insert (propertize string 'font-lock-face face-property)))

(defun arxiv-next-entry (&optional arg)
  "Move to the next arXiv entry.
With ARG, move to the next nth entry."
  (interactive "P")
  (setq arxiv-current-entry (+ arxiv-current-entry (prefix-numeric-value arg)))
  (let ((len (- (safe-length arxiv-entry-list) 1)))
    (when (>= arxiv-current-entry len)
      (if (eq arxiv-query-results-max arxiv-query-total-results)
	  (when (> arxiv-current-entry len)
	    (setq arxiv-current-entry (- (safe-length arxiv-entry-list) 1))
	    (message "end of search results"))
	(arxiv-show-next-page))))
  (goto-char (point-min))
  (forward-line (* 4 arxiv-current-entry))
  (move-overlay arxiv-highlight-overlay
		(point) (progn (beginning-of-line 5) (point)))
  (forward-line (- 4))
  (when arxiv-abstract-window
    (arxiv-show-abstract)))

(defun arxiv-prev-entry (&optional arg)
  "Move to the previous arXiv entry.
With ARG, move to the previous nth entry."
  (interactive "P")
  (setq arxiv-current-entry (- arxiv-current-entry (prefix-numeric-value arg)))
  (when (< arxiv-current-entry 0)
    (setq arxiv-current-entry 0)
    (message "beginning of search results"))
  (goto-char (point-min))
  (forward-line (* 4 arxiv-current-entry))
  (move-overlay arxiv-highlight-overlay
		(point) (progn (beginning-of-line 5) (point)))
  (forward-line (- 4))
  (when (window-live-p arxiv-abstract-window)
    (arxiv-show-abstract)))

(defun arxiv-select-entry ()
    "Select the entry to which the cursor is pointing to."
    (interactive)
    (setq arxiv-current-entry (/ (line-number-at-pos) 4))
    (goto-char (point-min))
    (forward-line (* 4 arxiv-current-entry))
    (move-overlay arxiv-highlight-overlay
		  (point) (progn (beginning-of-line 5) (point)))
    (forward-line (- 4))
    (when (window-live-p arxiv-abstract-window)
      (arxiv-show-abstract)))

(defun arxiv-click-select-entry (ev)
  "Select the entry at which the mouse is currently pointing.
This should be bound to a mouse click event EV."
  (interactive "e")
  (mouse-set-point ev)
  (arxiv-select-entry)
  (arxiv-show-abstract))

(defun arxiv-open-current-url ()
  "Open the webpage for the current highlighted paper entry."
  (interactive)
  (browse-url (cdr (assoc 'url (nth arxiv-current-entry arxiv-entry-list)))))

(defun arxiv-download-pdf (&optional confirm)
  "Download and save the highlighted paper to desired folder.
Return the path of the saved pdf file.  You can change the
default folder by customizing the variable
`arxiv-default-download-folder'.  If CONFIRM is t, don't prompt the
user with opening file."
  (interactive)
  (let ((url (cdr (assoc 'pdf (nth arxiv-current-entry arxiv-entry-list))))
	(newfile) (pdfname) (input))
    (string-match "/\\([^/]+?\\)$" url)
    (setq pdfname (concat (match-string 1 url) ".pdf"))
    (setq newfile (read-file-name "save pdf as: "
				  (expand-file-name arxiv-default-download-folder)
				  nil nil pdfname))
    (if (directory-name-p newfile) (setq newfile (concat newfile pdfname)))
    (url-copy-file url newfile 1)
    (unless confirm
      (setq input (read-char-exclusive (format "%s saved. Open pdf? (y/N) " newfile)))
      (when (or (equal input ?y) (equal input ?Y))
	(funcall arxiv-pdf-open-function newfile)))
    newfile))

(defun arxiv-customize ()
  "Customize the `arxiv-mode'."
  (interactive)
  (customize-group 'arxiv))

(defun arxiv-show-abstract ()
  "Show the abstract window and display appropriate information."
  (unless (buffer-live-p arxiv-abstract-buffer)
    (setq arxiv-abstract-buffer (get-buffer-create "*arXiv-abstract*")))
  (with-current-buffer arxiv-abstract-buffer
    (arxiv-abstract-mode)
    (setq-local prettify-symbols-alist arxiv-abstract-prettify-symbols-alist)
    (prettify-symbols-mode 1)
    (arxiv-format-abstract-page (nth arxiv-current-entry arxiv-entry-list)))
  (unless (window-live-p arxiv-abstract-window)
    (setq arxiv-abstract-window (display-buffer "*arXiv-abstract*" t))))
  
(defun arxiv-toggle-abstract ()
  "Toggle the visibility of the abstract.
If the abstract window does not exist, create it and display
 appropriate content, otherwise kill it."
  (interactive)
  (if (window-live-p arxiv-abstract-window)
      (with-selected-window arxiv-abstract-window
        (delete-window)
        (setq arxiv-abstract-window nil))
    (arxiv-show-abstract)))

(defun arxiv-SPC ()
  "Jump to the cursor position or open abstract window.
If the cursor position does not correspond to the current entry,
move the current entry to the corresponding position. Otherwise call
`arxiv-toggle-abstract'."
  (interactive)
  (if (eq (/ (line-number-at-pos) 4) arxiv-current-entry)
      (arxiv-toggle-abstract)
    (arxiv-select-entry)))

(defun arxiv-exit ()
  "Exit from the arXiv mode, deleting all relevant buffers."
  (interactive)
  (when (window-live-p arxiv-abstract-window)
    (quit-restore-window arxiv-abstract-window 'kill)
    (setq arxiv-abstract-window nil))
  (kill-buffer "*arXiv-update*")
  (when (get-buffer "*arXiv-abstract*")
    (kill-buffer "*arXiv-abstract*"))
  (setq arxiv-abstract-buffer nil))


(defun arxiv-headerline-format ()
  "Update the header line of *arxiv-update* buffer."
  (let* ((entry (format "%d/%d" (+ 1 arxiv-current-entry) arxiv-query-total-results))
	 (info-width (- (window-total-width) (length entry) 2)))
    (list
     (list (- info-width)
	   (if (or (eq arxiv-mode-entry-function 'arxiv-complex-search) (eq arxiv-mode-entry-function 'arxiv-search))
	       (concat " search results for " arxiv-query-info)
	     arxiv-query-info)
     (propertize " " 'display `(space :align-to ,info-width))
     entry))))

(defun arxiv-fill-page (&optional min-entry max-entry)
  "Fill the details of the arxiv article list according to `arxiv-entry-list'.
If MIN-ENTRY and MAX-ENTRY are ignored, defaults to fill with the whole `arxiv-entry-list'."
  (unless min-entry
    (setq min-entry 0))
  (let ((arxiv-entry-list-trun (seq-subseq arxiv-entry-list min-entry max-entry))) ; if max is omitted it defaults to be len(list)
    (mapcar
     (lambda (entry)
       (progn
	 (arxiv-insert-with-face (format  " %s\n " (alist-get 'title entry)) 'arxiv-title-face)
	 (let ((author-list (copy-sequence (alist-get 'author entry))) (overlength nil))
	   (when (> (length author-list) arxiv-author-list-maximum)
	     (setcdr (nthcdr (1- arxiv-author-list-maximum) author-list) nil)
	     (setq overlength t))
	   (dolist (author author-list)
	     (arxiv-insert-with-face (format "%s" author) 'arxiv-author-face)
	     (insert ", "))
	   (if overlength
	       (insert "et al.")
	     (delete-char -2)))
	 (let ((date (alist-get 'date entry)))
	   (string-match "^[-[:digit:]]+ " date)
	   (arxiv-insert-with-face (format "\n %s " (match-string 0 date)) 'arxiv-date-face))
	 (let ((cats (alist-get 'categories entry)))
	   (dolist (cat cats)
	     (arxiv-insert-with-face (format "[%s] " cat) 'arxiv-keyword-face)))
	 (insert "\n\n")))
     arxiv-entry-list-trun)))

(defun arxiv-populate-page ()
  "Populate the page of results according to `arxiv-entry-list' into buffer."
  (if arxiv-entry-list
      (progn
	(unless (buffer-live-p arxiv-buffer)
	  (setq arxiv-buffer (get-buffer-create "*arXiv-update*")))
	(with-current-buffer arxiv-buffer
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (arxiv-fill-page)
	  (goto-char (point-min))
	  (setq arxiv-current-entry 0)
	  (arxiv-mode)
	  (move-overlay arxiv-highlight-overlay
			(point) (progn (beginning-of-line 5) (point)))
	  (goto-char (point-min))
	  (message "Showing results %d-%d of %d" arxiv-query-results-min arxiv-query-results-max arxiv-query-total-results))
	(switch-to-buffer arxiv-buffer)
	(when arxiv-startup-with-abstract-window
	  (arxiv-show-abstract)))
    (message "No articles matching the search condition.")))

(defun arxiv-show-next-page ()
  "Perform one more query and fill the results into buffer.
\(according to `arxiv-current-entry' and `arxiv-entries-per-fetch')"
  (unless arxiv-buffer
    (setq arxiv-buffer (get-buffer "*arXiv-update*")))
  (let* ((min (* arxiv-entries-per-fetch (/ (+ 1 arxiv-current-entry) arxiv-entries-per-fetch)))
	 (max (+ min arxiv-entries-per-fetch)))
    (when (> max arxiv-query-total-results)
      (setq max arxiv-query-total-results))
    (message "Fetching results %d-%d..." (+ 1 min) max)
    (cond
     ((eq arxiv-mode-entry-function 'arxiv-read-new)
      (setq arxiv-entry-list
	    (append arxiv-entry-list
		    (arxiv-query (alist-get 'category arxiv-query-data-list)
				 (alist-get 'date-start arxiv-query-data-list)
				 (alist-get 'date-end arxiv-query-data-list)
				 min t))))
     ((eq arxiv-mode-entry-function 'arxiv-read-recent)
      (setq arxiv-entry-list
	    (append arxiv-entry-list
		    (arxiv-query (alist-get 'category arxiv-query-data-list)
				 (alist-get 'date-start arxiv-query-data-list)
				 (alist-get 'date-end arxiv-query-data-list)
				 min nil))))
     ((eq arxiv-mode-entry-function 'arxiv-read-author)
      (setq arxiv-entry-list
	    (append arxiv-entry-list
		    (arxiv-query-author (alist-get 'author arxiv-query-data-list)
					(alist-get 'category arxiv-query-data-list)
					min))))
     ((or (eq arxiv-mode-entry-function 'arxiv-complex-search) (eq arxiv-mode-entry-function 'arxiv-search))
      (setq arxiv-entry-list (append arxiv-entry-list (arxiv-query-general min)))))
    (set-buffer arxiv-buffer)
    (goto-char (point-max))
    (setq buffer-read-only nil)
    (arxiv-fill-page min)
    (setq buffer-read-only t)))

(defun arxiv-format-abstract-page (entry)
  "Format the arxiv abstract page according to ENTRY."
  ;; header-line
  (setq header-line-format (format " arXiv:%s" (cdr (assoc 'id entry))))
  ;;contents
  (let ((buffer-read-only nil))
    (erase-buffer)
    ;; title
    (arxiv-insert-with-face "\n" arxiv-title-face)
    (insert-button (format "%s" (cdr (assoc 'title entry)))
		   'action (lambda () (arxiv-open-current-url))
		   'face 'arxiv-abstract-title-face
		   'mouse-face 'highlight
		   'follow-link t
		   'help-echo (format "Link: %s" (cdr (assoc 'url entry))))
    (arxiv-insert-with-face "\n\n" arxiv-title-face)
    ;; author list
    (let ((author-list (cdr (assoc 'author entry))))
      (dolist (author author-list)
	(insert-button (format "%s" author)
		       'action (lambda () (arxiv-toggle-abstract) (arxiv-read-author author))
		       'follow-link t
		       'face 'arxiv-abstract-author-face
		       'mouse-face 'highlight
		       'help-echo (format "Look up author: %s" author))
	(insert ", ")))
    (delete-char -2)
    (insert "\n\n")
    ;; abstract
    (let ((abstract (cdr (assoc 'abstract entry))))
      (arxiv-insert-with-face "    " arxiv-abstract-face)
      (setq abstract (replace-regexp-in-string "^ +" "" abstract))
      (insert (propertize abstract 'font-lock-face arxiv-abstract-face 'wrap-prefix "    ")))
    ;; highlight math
    (save-excursion
      (while (search-backward-regexp "\\$[^$]+\\$" nil t)
	(add-text-properties (match-beginning 0) (match-end 0) '(font-lock-face arxiv-abstract-math-face))))
    ;; comment
    (if (cdr (assoc 'comment entry))
	(arxiv-insert-with-face (format "\n\nComments: %s" (cdr (assoc 'comment entry))) arxiv-subfield-face)
      (arxiv-insert-with-face "\n\nComments: N/A" arxiv-subfield-face))
    ;; subject
    (arxiv-insert-with-face (format "\nSubjects: ") arxiv-subfield-face)
    (let* ((main-cat t) (cats (cdr (assoc 'categories entry))))
      (dolist (cat cats)
	(let (field)
	  (setq field (cdr (assoc (intern-soft cat) arxiv-subject-classifications)))
	  (if main-cat
	      (progn ; the main subject is in bold
		(arxiv-insert-with-face (format "%s " field) '(:inherit arxiv-subfield-face :weight semi-bold))
		(arxiv-insert-with-face (format "(%s)" cat) '(:inherit arxiv-subfield-face :weight semi-bold))
		(setq main-cat nil))
	    (insert (propertize (format "%s " field) 'font-lock-face arxiv-subfield-face 'wrap-prefix "  "))
	    (insert (propertize (format "(%s)" cat) 'font-lock-face arxiv-subfield-face 'wrap-prefix "  ")))
	  (arxiv-insert-with-face "; " arxiv-subfield-face))))
    (delete-char -2)
    ;; journal/DOI
    (when (cdr (assoc 'journal entry)) (arxiv-insert-with-face (format "\nJournal: %s" (cdr (assoc 'journal entry))) arxiv-subfield-face))
    (when (cdr (assoc 'DOI entry)) (arxiv-insert-with-face (format "\nDOI: %s" (cdr (assoc 'DOI entry))) arxiv-subfield-face))
    ;; times
    (arxiv-insert-with-face (format "\nSubmitted: %s" (cdr (assoc 'date entry))) arxiv-subfield-face)
    (arxiv-insert-with-face (format "\nUpdated: %s" (cdr (assoc 'updated entry))) arxiv-subfield-face)))

(defun arxiv-export-bibtex-to-string (&optional pdfpath)
  "Generate a bibtex entry according to the current arxiv entry.
Also add a link to PDFPATH in bibtex entry if it is specified.
It returns a string buffer containing the bibtex entry. This
function is a part of arXiv mode, and is supposed to be called by
`arxiv-export-bibtex' or `arxiv-export-bibtex-to-buffer'."
  (let*
      ((entry (nth arxiv-current-entry arxiv-entry-list))
       (title (cdr (assoc 'title entry)))
       (id (cdr (assoc 'id entry)))
       (author-list (cdr (assoc 'author entry)))
       (abstract (cdr (assoc 'abstract entry)))
       (year (cdr (assoc 'date entry)))
       (url (cdr (assoc 'url entry)))
       (journal (cdr (assoc 'journal entry)))
       (doi (cdr (assoc 'doi entry)))
       (author) (key) (bibtex-info))
    (setq author-list (mapcar (lambda (name) (progn
			(string-match "\\(.+\\) \\([^ ]+?\\)$" name)
			(setq name (concat (match-string 2 name) ", " (match-string 1 name)))))
			       author-list))
    (string-match "^[0-9]+" year)
    (setq year (match-string 0 year))
    (setq author (mapconcat #'identity author-list " and "))
    (setq abstract (replace-regexp-in-string "^ +" "" abstract))
    (setq abstract (replace-regexp-in-string " +$" "" abstract))
    (setq bibtex-info (format "@article{,
title = {%s},
author = {%s},
year = {%s}
}" title author year))
    (with-temp-buffer
      (insert bibtex-info)
      (bibtex-mode)
      (bibtex-set-dialect 'BibTeX t)
      (setq key (bibtex-generate-autokey)))
    (setq bibtex-info (format "@article{%s,
title = {%s},
author = {%s},
abstract = {%s},
archivePrefix = {arXiv},
eprint = {%s},
url = {%s},
year = {%s}" key title author abstract id url year))
    (when doi
      (setq bibtex-info (concat bibtex-info (format ",\ndoi = {%s}" doi))))
    (when journal
      (setq bibtex-info (concat bibtex-info (format ",\njournal = {%s}" journal))))
    (when pdfpath
      (setq bibtex-info (concat bibtex-info (format ",\nfile = {:%s:pdf}" (expand-file-name pdfpath)))))
    (setq bibtex-info (concat bibtex-info "\n}"))
    bibtex-info))

(defun arxiv-export-bibtex (&optional pdfpath)
  "Add a new bibtex item to a .bib file according to the current arxiv entry.
Also add a link to PDFPATH in bibtex entry if it is specified.
This function is a part of arXiv mode. You can
customize the default .bib file by customizing the
`arxiv-default-bibliography' variable."
  (interactive)
  (let ((bibtex-info (arxiv-export-bibtex-to-string pdfpath))
	(bibfile (read-file-name "export to bibliography file: " nil nil t (expand-file-name arxiv-default-bibliography))))
    (with-temp-buffer
      (find-file bibfile)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (insert bibtex-info)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (save-buffer))))

(defun arxiv-export-bibtex-to-buffer (&optional pdfpath)
  "Export bibtex for the current entry and display it in a temporary buffer.
Also add a link to PDFPATH in bibtex entry if it is specified.
This function is a part of arXiv mode, and
is not related to the arxiv-add-bibtex-entry in org-ref package."
  (interactive)
  (let
      ((bibtex-info (arxiv-export-bibtex-to-string pdfpath)))
    (select-window arxiv-abstract-window)
    (pop-to-buffer "*arXiv-bibTeX*" '(display-buffer-below-selected))
    ;; (split-window-below)
    ;; (select-window (next-window))
    ;; (pop-to-buffer-same-window "*arXiv-bibTeX*")
    (erase-buffer)
    (insert bibtex-info)
    (setq buffer-read-only nil)
    (bibtex-mode)
    (bibtex-set-dialect 'BibTeX t)))

(defun arxiv-download-pdf-export-bibtex ()
  "Download the pdf file of the current entry and export a bibtex entry to the selected bibtex file."
  (interactive)
  (arxiv-export-bibtex (arxiv-download-pdf t)))


;; Entry functions

;;;###autoload
(defun arxiv-read-new (&optional cat res-time)
  "Read new (submitted in the previous work day) arXiv articles in given CAT.
If CAT is not supplied, prompt user for category.
With optional arg RES-TIME, read new submission with respective to it."
  (interactive)
  (let*
      ((time (or res-time (current-time)))
       (TZ "EST+5EDT,M3.2.0/2,M11.1.0/2") ;; arXiv (Cornell) is based on eastern time (ET)
       (day (string-to-number (format-time-string "%u" time TZ)))
       (date-start nil)
       (date-end nil)
       (dayname-start "")
       (dayname-end "")
       (category (or cat
		     (completing-read "Select category: " arxiv-categories nil t nil nil arxiv-default-category))))
    
    ;; arxiv announces new submissions on 20:00 ET. Check if it's 20:00 yet.
    (when (< (string-to-number (format-time-string "%H" time TZ)) 20)
      (setq day (- day 1))
      (setq time (time-subtract time 86400)))
    
    ;; new submission deadlines are 14:00 ET
    (let* ((dectime (decode-time time TZ))
	   (daysec (+ (* (decoded-time-hour dectime) 3600) (* (decoded-time-minute dectime) 60) (decoded-time-second dectime)))
	   (timediff (- daysec (* 14 3600))))
      (setq time (time-subtract time timediff)))
            
    ;; time for submissions are listed in UTC
    (cond
     ((equal day 5) ; Friday
      (progn
	(setq date-start (format-time-string "%Y%m%d%H%M" (time-subtract time (* 2 86400)) "UTC"))
	(setq date-end (format-time-string "%Y%m%d%H%M" (time-subtract time 86400) "UTC"))
	(setq dayname-start "Wed")
	(setq dayname-end "Thu")))
     ((equal day 6) ; Saturday
      (progn
	(setq date-start (format-time-string "%Y%m%d%H%M" (time-subtract time (* 3 86400)) "UTC"))
	(setq date-end (format-time-string "%Y%m%d%H%M" (time-subtract time (* 2 86400)) "UTC"))
	(setq dayname-start "Wed")
	(setq dayname-end "Thu")))
     ((or (equal day 7) (eq day 0)) ; Sunday
      (progn
	(setq date-start (format-time-string "%Y%m%d%H%M" (time-subtract time (* 3 86400)) "UTC"))
	(setq date-end (format-time-string "%Y%m%d%H%M" (time-subtract time (* 2 86400)) "UTC"))
	(setq dayname-start "Thu")
	(setq dayname-end "Fri")))
     ((equal day 1) ; Monday
      (progn
	(setq date-start (format-time-string "%Y%m%d%H%M" (time-subtract time (* 3 86400)) "UTC"))
	(setq date-end (format-time-string "%Y%m%d%H%M" time "UTC"))
	(setq dayname-start "Fri")
	(setq dayname-end "Mon")))
     (t ; Tue - Thu, read from previous day
      (progn
	(setq date-start (format-time-string "%Y%m%d%H%M" (time-subtract time 86400) "UTC"))
	(setq date-end (format-time-string "%Y%m%d%H%M" time "UTC"))
	(setq dayname-start (format-time-string "%a" (time-subtract time 86400) "UTC"))
	(setq dayname-end (format-time-string "%a" time "UTC")))))
    ;; day to week name
    (setq arxiv-query-info (format " Showing new submissions in %s from %s(%s) to %s(%s)."
				   category (substring date-start 0 8) dayname-start (substring date-end 0 8) dayname-end))
    (setq arxiv-entry-list (arxiv-query category date-start date-end nil t))
    (setq arxiv-query-data-list `((date-start . ,date-start) (date-end . ,date-end) (category . ,category)))
    (setq arxiv-mode-entry-function 'arxiv-read-new)
    (if arxiv-entry-list
	(arxiv-populate-page)
      (when (y-or-n-p "Could not find any new submissions (this often happens because there is often a slight delay between arXiv's said and actual announcement time).
Read from previous day instead? ")
	(arxiv-read-new category (time-subtract (current-time) 86400))))))

;;;###autoload
(defun arxiv-read-recent ()
  "Read recent (past week) submissions of arXiv in a given category."
  (interactive)
  (let*
      ((date-end (format-time-string "%Y%m%d" (current-time)))
       (date-start (format-time-string "%Y%m%d" (time-subtract (current-time) (* 7 86400))))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
    (setq arxiv-query-info (format " Showing recent submissions in %s in the past week (%s to %s)." category date-start date-end))
    (setq date-start (concat date-start "0000"))
    (setq date-end (concat date-end "0000"))
    (setq arxiv-entry-list (arxiv-query category date-start date-end))
    (setq arxiv-query-data-list `((date-start . ,date-start) (date-end . ,date-end) (category . ,category)))
    (setq arxiv-mode-entry-function 'arxiv-read-recent)
    (arxiv-populate-page)))

;;;###autoload
(defun arxiv-read-author (&optional author)
  "Find the papers by author name in category supplied by user.
If AUTHOR is non-nil, find papers by author in all categories."
  (interactive)
  (let ((category nil))
    (unless author
      (setq author (read-string "Authors name (use space to separate): "))
      (setq category (completing-read "Select category: " arxiv-categories nil t nil nil arxiv-default-category)))
    (setq arxiv-entry-list (arxiv-query-author author category))
    (setq category (or category "all"))
    (setq arxiv-query-info (format " Showing results for author(s): %s in categroy %s." author category))
    (setq arxiv-query-data-list `((author . ,author) (category . ,category)))
    (setq arxiv-mode-entry-function 'arxiv-read-author)
    (arxiv-populate-page)))

;;;###autoload
(defun arxiv-search ()
  "Do a simple search on arXiv datebase and list the result in buffer."
  (interactive)
  (let ((condition (read-string "Search all fields (use space to seperate and \"\" to quote): ")))
    (if (string-match "^ *$" condition)
	(message "exit with blank search condition.")
      (setq arxiv-query-data-list `((all t ,condition)))
      (setq arxiv-query-info (format "all:%s" condition))
      (setq arxiv-entry-list (arxiv-query-general))
      (setq arxiv-mode-entry-function 'arxiv-search)
      (arxiv-populate-page))))

;;;###autoload
(defun arxiv-complex-search ()
  "Do a complex search on arXiv database and list the result in buffer."
  (interactive)
  (setq arxiv-query-data-list nil)
  (setq arxiv-query-info "")
  (arxiv-search-menu/body))

(defun arxiv-refine-search ()
  "Refine search conditions in the *arXiv-update* buffer."
  (interactive)
  (if (or (eq arxiv-mode-entry-function 'arxiv-complex-search) (eq arxiv-mode-entry-function 'arxiv-search))
      (progn
	(message "refine search condition: ")
	(arxiv-search-menu/body))
    (message "Refining search is only available in arxiv-search or arxiv-complex-search.")))

(defun arxiv-query-data-update (field condition)
  "Update the variable `arxiv-query-data-list' in FIELD.
Do exclusive update if CONDITION is nil. Also updates `arxiv-query-info'."
  (if (or condition arxiv-query-data-list)
      (let ((temp-query-info) (context))
	(if condition
	    (setq temp-query-info "+")
	  (setq temp-query-info "-"))
	(cond
	 ((eq field 'all)
	  (progn
	    (setq context (read-string "Search all fields (use space to seperate and \"\" to quote): "))
	    (setq temp-query-info (concat temp-query-info "all:" context))))
	 ((eq field 'id)
	  (progn
	    (setq context (read-string "Article ID: "))
	    (setq temp-query-info (concat temp-query-info "ID:" context))))
	 ((eq field 'author)
	  (progn
	    (setq context (read-string "Authors name (use space to seperate): "))
	    (setq temp-query-info (concat temp-query-info "author:" context))))
	 ((eq field 'abstract)
	  (progn
	    (setq context (read-string "Abstract keywords (use space to seperate and \"\" to quote): "))
	    (setq temp-query-info (concat temp-query-info "abstract:" context))))
	 ((eq field 'category)
	  (progn
	    (setq context (completing-read "Category: " arxiv-categories nil t nil nil arxiv-default-category))
	    (setq temp-query-info (concat temp-query-info "category:" context))))
	 ((eq field 'journal)
	  (progn
	    (setq context (read-string "Journal: "))
	    (setq temp-query-info (concat temp-query-info "journal:" context))))
	 ((eq field 'comment)
	  (progn
	    (setq context (read-string "Search comments (use space to seperate and \"\" to quote): "))
	    (setq temp-query-info (concat temp-query-info "comment:" context))))
	 ((eq field 'time)
	  (let
	      ((date-min (read-string "Enter starting date (YYYYMMDD, ex: 19910101): "))
	       (date-max (read-string "Enter ending date (YYYYMMDD, ex: 19910101): ")))
	    (setq context (format "[%s0000+TO+%s0000]" date-min date-max))
	    (setq temp-query-info (concat temp-query-info "time:" (format "%s-%s" date-min date-max))))))
	(if (string-match "^ *$" context)
	    (message "Void search argument.")
	  (setq arxiv-query-info (concat arxiv-query-info temp-query-info))
	  (setq arxiv-query-data-list (cons (list field condition context) arxiv-query-data-list)))) ; this reversed the order of the list, need to fix it later on
    (message "Only inclusive searching is allowed as the first keyword."))
  (arxiv-search-menu/body))

(defun arxiv-hydra-perform-search ()
  "Helper function for `arxiv-search-menu'."
  (interactive)
  (if arxiv-query-data-list
      (progn
	(setq arxiv-query-info (replace-regexp-in-string "^+" "" arxiv-query-info))
	(setq arxiv-query-data-list (nreverse arxiv-query-data-list)) ; fix the reverse order caused in qrxiv-query-data-update ()
	(setq arxiv-entry-list (arxiv-query-general))
	(setq arxiv-mode-entry-function 'arxiv-complex-search)
	(arxiv-populate-page))
    (message "quit with blank search conditions")))


(defhydra arxiv-search-menu (:color blue :foreign-keys warn :exit t)
  "
Condition: %`arxiv-query-info
Add search condition:
-------------------------------------------------------------------------------
_a_: all                   _i_: article ID            _t_: submitted time
_u_: author(s)             _b_: abstract              _c_: category
_j_: journal               _m_: comment               _-_: exclude condition
_x_: perform search with current condition(s)       _q_: quit
"
  ("a" (arxiv-query-data-update 'all t))
  ("i" (arxiv-query-data-update 'id t))
  ("t" (arxiv-query-data-update 'time t))
  ("u" (arxiv-query-data-update 'author t))
  ("b" (arxiv-query-data-update 'abstract t))
  ("c" (arxiv-query-data-update 'category t))
  ("j" (arxiv-query-data-update 'journal t))
  ("m" (arxiv-query-data-update 'comment t))
  ("-" arxiv-search-menu-ex/body)
  ("x" arxiv-hydra-perform-search)
  ("q" (setq arxiv-query-data-list nil) "quit"))
 
(defhydra arxiv-search-menu-ex (:color red :foreign-keys warn :exit t)
  "
Condition: %`arxiv-query-info
Exclude arXiv search condition:
-------------------------------------------------------------------------------
_a_: all                   _i_: article ID            _t_: submitted time
_u_: author(s)             _b_: abstract              _c_: category
_j_: journal               _m_: comment               _+_: include condition
_x_: perform search with current condition(s)       _q_: quit
"
  ("a" (arxiv-query-data-update 'all nil))
  ("i" (arxiv-query-data-update 'id nil))
  ("t" (arxiv-query-data-update 'time nil))
  ("u" (arxiv-query-data-update 'author nil))
  ("b" (arxiv-query-data-update 'abstract nil))
  ("c" (arxiv-query-data-update 'category nil))
  ("j" (arxiv-query-data-update 'journal nil))
  ("m" (arxiv-query-data-update 'comment nil))
  ("+" arxiv-search-menu/body)
  ("x" arxiv-hydra-perform-search)
  ("q" (setq arxiv-query-data-list nil) "quit"))

(defhydra arxiv-help-menu (:color red :foriegn-keys run)
  "
ArXiv mode help message
---------------------------------------------------------------------------------------------------------
_n_: next entry           _SPC_: toggle abstract window          _b_: export bibtex entry
_p_: previous entry       _RET_: open link in browser            _B_: display bibtex entry in new buffer
_r_: refine search          _d_: download PDF                    _e_: download pdf & export bibtex entry
_q_: quit Arxiv mode        _?_: toggle this help
"
  ("n" arxiv-next-entry)
  ("p" arxiv-prev-entry)
  ("r" arxiv-refine-search :exit t)
  ("q" arxiv-exit :exit t)
  ("SPC" arxiv-SPC)
  ("RET" arxiv-open-current-url)
  ("d" arxiv-download-pdf)
  ("?" nil)
  ("b" arxiv-export-bibtex)
  ("B" arxiv-export-bibtex-to-buffer)
  ("e" arxiv-download-pdf-export-bibtex))

(provide 'arxiv-mode)

;;; arxiv-mode.el ends here

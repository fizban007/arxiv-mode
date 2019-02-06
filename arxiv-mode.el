;; Defines the arxiv-mode
;; 
;; Author: Alex Chen (fizban007)
;; Email: fizban007 (at) gmail (dot) com
;;
;; Modified by Simon Lin (iserlohn)
;; Email: n.sibetz@gmail.com
;;
;; This software is distributed under GPL license
;;
;;

(require 'overlay)
(require 'button)
(require 'org)
(require 'hydra)
(require 'arxiv-vars)
(require 'arxiv-query)
(require 'arxiv-abstract)

;; Current window for viewing the arXiv abstract
(setq arxiv-abstract-window nil)

;; Overlays for highlighting selections
;; TODO: Might need to weed some unused overlays
(aset arxiv-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref arxiv-highlight-overlays 0)
             'face 'highlight)

(aset arxiv-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref arxiv-highlight-overlays 1)
             'face 'font-lock-keyword-face)

(aset arxiv-highlight-overlays 2 (make-overlay 1 1))
(overlay-put (aref arxiv-highlight-overlays 2)
             'face 'font-lock-keyword-face)

(defun arxiv-remove-highlight (index)
  "Remove highlighting an entry"
  (delete-overlay (aref arxiv-highlight-overlays index)))

(defun arxiv-highlight-entry (index begin &optional buffer)
  "Highlight an entry with overlay INDEX"
  (setq end (re-search-forward "^\n"))
  (move-overlay (aref arxiv-highlight-overlays index)
                begin end (or buffer (current-buffer)))
  (re-search-backward "^Title:"))

(defun arxiv-next-entry (&optional arg)
  "Move to the next arXiv entry"
  (interactive "P")
  (unless arg
    (setq arg 1))
  (while (and (> arg 0)
              (< arxiv-current-entry (+ (safe-length arxiv-entry-list) -1)))
    (progn
      ;; (setq arxiv-current-entry (+ arxiv-current-entry 1))
      (setq arg (+ arg -1))
      (arxiv-remove-highlight 0)
      (or (eobp) (forward-char 1))
      (re-search-forward "^Title:" nil t nil)
      (beginning-of-line 1)
      (arxiv-highlight-entry 0 (point))))
  (setq arxiv-current-entry (/ (line-number-at-pos (point)) 4))
  (when arxiv-abstract-window
    (arxiv-show-abstract)))

(defun arxiv-prev-entry (&optional arg)
  "Move to the previous arXiv entry"
  (interactive "P")
  (unless arg
    (setq arg 1))
  (while (and (> arg 0)
              (> arxiv-current-entry 0))
    (progn
      ;; (setq arxiv-current-entry (+ arxiv-current-entry -1))
      (setq arg (+ arg -1))
      (arxiv-remove-highlight 0)
      (re-search-backward "^Title:" nil t nil)
      (beginning-of-line 1)
      (arxiv-highlight-entry 0 (point))))
  (setq arxiv-current-entry (/ (line-number-at-pos (point)) 4))
  (when arxiv-abstract-window
    (arxiv-show-abstract)))

(defun arxiv-open-current-url ()
  "Open the webpage for the current highlighted paper entry."
  (interactive)
  (setq url (cdr (assoc 'url (nth arxiv-current-entry arxiv-entry-list))))
  ;; (start-process "arxiv-webpage" nil arxiv-default-browser url)
  (browse-url url))

(defun arxiv-download-pdf (&optional confirm)
  "Download and save the highlighted paper to desired folder.
Return the path of the saved pdf file.
You can change the default folder by customizing the variable arxiv-default-download-folder.
If the optional argument is t, don't prompt the user with opening file."
  (interactive)
  (let ((url (cdr (assoc 'pdf (nth arxiv-current-entry arxiv-entry-list))))
	(newfile) (pdfname) (input))
    (string-match "/\\([^/]+?\\)$" url)
    (setq pdfname (concat (match-string 1 url) ".pdf"))
    (setq newfile (read-file-name "save pdf as: "
				  (expand-file-name arxiv-default-download-folder)
				  pdfname nil pdfname))
    (if (directory-name-p newfile) (setq newfile (concat newfile pdfname)))
    (url-copy-file url newfile 1)
    (unless confirm
      (setq input (read-char-exclusive (format "%s saved. Open pdf? (y/N) " newfile)))
      (when (or (equal input ?y) (equal input ?Y))
	(funcall arxiv-pdf-open-function newfile)))
    newfile))

(defun arxiv-customize ()
  "Customize the arxiv-mode"
  (interactive)
  (customize-group 'arxiv))

(defun arxiv-show-abstract ()
  (unless arxiv-abstract-window
    (setq abstract-window (split-window-right)))
  (setq abstract-buffer (get-buffer-create "*arXiv-abstract*"))
  (with-selected-window abstract-window
    (switch-to-buffer abstract-buffer)
    (set-buffer abstract-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (arxiv-abstract-mode)    
    (arxiv-format-abstract-page (nth arxiv-current-entry arxiv-entry-list))
    (setq-local prettify-symbols-alist arxiv-abstract-prettify-symbols-alist)
    (prettify-symbols-mode 1)
    (when tabbar-mode (tabbar-local-mode 1))
    (setq buffer-read-only t))
  (setq arxiv-abstract-window (get-buffer-window abstract-buffer)))
  
(defun arxiv-show-hide-abstract (&optional arg)
  "Toggle the visibility of the abstract. If the abstract window
  does not exist, then create it and display appropriate content,
  otherwise kill it."
  (interactive)
  (if arxiv-abstract-window
      (with-selected-window arxiv-abstract-window
        (delete-window)
        (setq arxiv-abstract-window nil))
    (arxiv-show-abstract)))
 
(defun arxiv-exit (&optional arg)
  "Exit from the arXiv mode, deleting all relevant buffers."
  (interactive)
  (when arxiv-abstract-window
    (delete-window arxiv-abstract-window)
    (setq arxiv-abstract-window nil))
  (kill-buffer "*arXiv-update*")
  (when (get-buffer "*arXiv-abstract*")
    (kill-buffer "*arXiv-abstract*")))

(setq arxiv-mode-map (make-sparse-keymap))
(define-key arxiv-mode-map "p" 'arxiv-prev-entry)
(define-key arxiv-mode-map "n" 'arxiv-next-entry)
(define-key arxiv-mode-map (kbd "RET") 'arxiv-open-current-url)
(define-key arxiv-mode-map (kbd "SPC") 'arxiv-show-hide-abstract)
(define-key arxiv-mode-map "d" 'arxiv-download-pdf)
(define-key arxiv-mode-map "e" 'arxiv-download-pdf-export-bibtex)
(define-key arxiv-mode-map "b" 'arxiv-export-bibtex-entry)
(define-key arxiv-mode-map "r" 'arxiv-refine-search)
(define-key arxiv-mode-map "q" 'arxiv-exit)
(define-key arxiv-mode-map (kbd "?") 'arxiv-help-menu/body)

(setq arxiv-keyword-list-default
      '(("Title:\\(.*?\\)$" . (1 arxiv-title-face))
        ("Title\\|Authors\\|Date" . arxiv-keyword-face)))

(defvar arxiv-syntax-table nil 
  "Syntax table for `arxiv-mode'.")

(defun arxiv-mode ()
  "Major mode for reading arXiv updates online."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'arxiv-mode)
  (setq mode-name "arXiv")
  ;; (make-local-variable 'paragraph-separate)
  ;; (make-local-variable 'paragraph-start)
  (make-local-variable 'page-delimiter)
  ;; (setq paragraph-start "^Title:")
  ;; (setq paragraph-separate " [ \t\^L]*$")
  (setq page-delimiter "^Title: ")
  (setq font-lock-defaults '(arxiv-keyword-list-default))
  (set-syntax-table arxiv-mode-syntax-table)
  (use-local-map arxiv-mode-map)
  (setq header-line-format '(:eval (arxiv-headerline-format)))
  ;; (setq font-lock-multiline t)
  (run-mode-hooks 'arxiv-mode-hook))

(defun arxiv-headerline-format ()
  "update the header line of *arxiv-update* buffer."
  (let* ((entry (format "%d/%d" (+ 1 arxiv-current-entry) arxiv-query-total-results))
	 (info-width (- (window-total-width) (length entry) 2)))
    (list
     (list (- info-width)
	   (if (eq arxiv-mode-entry-function 'arxiv-complex-search)
	       (concat " search results for " arxiv-query-info)	     
	     arxiv-query-info)
     (propertize " " 'display `(space :align-to ,info-width))
     entry))))

(defun arxiv-populate-page (page num-per-page &optional arxiv-buffer)
  "Populate the page of results according to arxiv-entry-list."
  (if arxiv-entry-list
      (progn
	(unless arxiv-buffer
	  (setq arxiv-buffer (get-buffer-create "*arXiv-update*")))
	(save-excursion
	  (set-buffer arxiv-buffer)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (mapcar
	   (lambda (entry)
	     (progn 
	       (insert (format "Title: %s\nAuthors: " (cdr (assoc 'title entry))))
	       (let ((authors (cdr (assoc 'authors entry))))
		 (while authors
		   (progn 
		     (insert (format "%s" (car authors)))
		     (setq authors (cdr authors))
		     (if authors
			 (insert ", "))
		     )))
	       (insert (format "\nDate: %s\n\n" (cdr (assoc 'date entry))))))
	   arxiv-entry-list)
	  (goto-char (point-min))
	  (arxiv-highlight-entry 0 (point))
	  (setq arxiv-current-entry 0)
	  (arxiv-mode)
	  (message "Showing results %d-%d of %d" arxiv-query-results-min arxiv-query-results-max arxiv-query-total-results)
	  (setq buffer-read-only t))
	(switch-to-buffer arxiv-buffer))
    (message "No articles at this time.")))

(defun arxiv-export-bibtex (&optional pdfpath)
  "Add a new bibtex item to a .bib file according to the current arxiv entry.
This function is a part of arXiv mode.
You can customize the default .bib file by customizing the arxiv-default-bibliography variable.
This function is not related to the arxiv-add-bibtex-entry in org-ref package."
  (interactive)
  (let*
      ((entry (nth arxiv-current-entry arxiv-entry-list))
       (title (cdr (assoc 'title entry)))
       (id (cdr (assoc 'id entry)))
       (author-list (cdr (assoc 'authors entry)))
       (abstract (cdr (assoc 'abstract entry)))
       (year (cdr (assoc 'date entry)))
       (url (cdr (assoc 'url entry)))
       (journal (cdr (assoc 'journal entry)))
       (doi (cdr (assoc 'doi entry)))
       (authors nil) (key nil) (bibtex-info nil) (bibfile nil))
    (setq author-list (mapcar (lambda (name) (progn
			(string-match "\\(.+\\) \\([^ ]+?\\)$" name)
			(setq name (concat (match-string 2 name) ", " (match-string 1 name)))))
			       author-list))
    (string-match "^[0-9]+" year)
    (setq year (match-string 0 year))
    (setq authors (string-join author-list " and "))
    (setq abstract (replace-regexp-in-string "^ +" "" abstract))
    (setq abstract (replace-regexp-in-string " +$" "" abstract))
    (setq bibtex-info (format "@article{,
title = {%s},
author = {%s},
year = {%s}
}" title authors year))
    (with-temp-buffer
      (insert bibtex-info)
      (bibtex-mode)
      (setq key (bibtex-generate-autokey)))
    (string-match "^@article{,\n\\(.+\\)" bibtex-info)
    (setq bibtex-info (format "@article{%s,
title = {%s},
author = {%s},
abstract = {%s},
archivePrefix = {arXiv},
eprint = {%s},
url = {%s},
year = {%s}" key title authors abstract id url year))
    (when doi
      (setq bibtex-info (concat bibtex-info (format ",\ndoi = {%s}" doi))))
    (when journal
      (setq bibtex-info (concat bibtex-info (format ",\njournal = {%s}" journal))))
    (when pdfpath
      (setq bibtex-info (concat bibtex-info (format ",\nfile = {:%s:pdf}" (expand-file-name pdfpath)))))
    (setq bibtex-info (concat bibtex-info "\n}"))
    (setq bibfile (read-file-name "export to bibliography file: " nil nil t (expand-file-name arxiv-default-bibliography)))
    (save-window-excursion
      (find-file bibfile)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (insert bibtex-info)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (save-buffer))))

(defun arxiv-download-pdf-export-bibtex ()
  "Download the pdf file of the current entry and export a bibtex entry to the selected bibtex file."
  (interactive)
  (arxiv-export-bibtex (arxiv-download-pdf t)))

(defun arxiv-read-new ()
  "read new (submitted in the previous work day) arXiv articles in a given category."
  (interactive)
  (let*
      ((day (string-to-number (format-time-string "%u" nil "EST")))
;       (announced nil)
       (date-start nil)
       (date-end nil)
       (dayname-start "")
       (dayname-end "")
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
    ;; arxiv announces new submssions on 20:00 EST. Check it's 20:00 yet.
    ;; Edit: Seems that the API database is only updated after midnight. No need to be so critical.
    ;; (when (< (string-to-number (format-time-string "%H" nil "EST")) 20)
    ;;   (setq day (- day 1))
    ;;   (setq announced t))
    (cond
     ((or (equal day 5) (equal day 6) (equal day 7))
      (progn
	(setq date-start (format-time-string "%Y%m%d" (org-read-date t t "-Wed")))
	(setq date-end (format-time-string "%Y%m%d" (org-read-date t t "-Thu")))
	(setq dayname-start "Wed")
	(setq dayname-end "Thu")))
     ((equal day 1)
      (progn
	(setq date-start (format-time-string "%Y%m%d" (org-read-date t t "-Thu")))
	(setq date-end (format-time-string "%Y%m%d" (org-read-date t t "-Fri")))
	(setq dayname-start "Thu")
	(setq dayname-end "Fri")))
     ((equal day 2)
      (progn
	(setq date-start (format-time-string "%Y%m%d" (org-read-date t t "-Fri")))
	(setq date-end (format-time-string "%Y%m%d" (org-read-date t t "-Mon")))
	(setq dayname-start "Fri")
	(setq dayname-end "Mon")))
     ((equal day 3)
      (progn
	(setq date-start (format-time-string "%Y%m%d" (org-read-date t t "-Mon")))
	(setq date-end (format-time-string "%Y%m%d" (org-read-date t t "-Tue")))
	(setq dayname-start "Mon")
	(setq dayname-end "Tue")))
     ((equal day 4)
      (progn
	(setq date-start (format-time-string "%Y%m%d" (org-read-date t t "-Tue")))
	(setq date-end (format-time-string "%Y%m%d" (org-read-date t t "-Wed")))
	(setq dayname-start "Tue")
	(setq dayname-end "Wed"))))
    ;; day to week name
    (setq arxiv-query-info (format " Showing new submissions in %s from %s(%s) to %s(%s) (EST)."
				   category date-start dayname-start date-end dayname-end))
    (setq date-start (concat date-start "1400"))
    (setq date-end (concat date-end "1400"))
    (setq arxiv-entry-list (arxiv-query category date-start date-end))
    (setq arxiv-mode-entry-function 'arxiv-read-new)
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-read-recent ()
  "read recent (past week) submissions of arXiv in a given category."
  (interactive)
  (let*
      ((date-start (format-time-string "%Y%m%d" (org-read-date nil t "-7")))
       (date-end (format-time-string "%Y%m%d" (org-read-date nil t "")))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
    (setq arxiv-query-info (format " Showing recent submissions in %s in the past week (%s to %s)." category date-start date-end))
    (setq date-start (concat date-start "0000"))
    (setq date-end (concat date-end "0000"))
    (setq arxiv-entry-list (arxiv-query category date-start date-end))
    (setq arxiv-mode-entry-function 'arxiv-read-recent)
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-read-author ()
  "Find the papers by author name."
  (interactive)
  (let*
      ((author (read-string "Authors name (use space to seperate): "))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
    (setq arxiv-entry-list (arxiv-query-author author category))
    (setq arxiv-query-info (format " Showing results for author(s): %s in categroy %s." author category))
    (setq arxiv-mode-entry-function 'arxiv-read-author)
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-complex-search ()
  "Do a complex search on arXiv database and list the result in buffer."
  (interactive)
  (setq arxiv-query-data-list nil)
  (setq arxiv-query-info "")
  (arxiv-search-menu/body))

(defun arxiv-refine-search ()
  "Refine search conditions in the *arXiv-update* buffer."
  (interactive)
  (if (eq arxiv-mode-entry-function 'arxiv-complex-search)
      (progn
	(message "refine search condition: ")
	(arxiv-search-menu/body))
    (message "Refining search function is only available in M-x arxiv-complex-search.")))

(defun arxiv-query-data-update (field condition)
  "Ask and update the variable arxiv-query-data-list in the corresponding search field.
Do exclusive update if condition is nil. Also updates arxiv-query-info."
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
	      ((date-min (replace-regexp-in-string "-" "" (org-read-date nil nil nil "Enter starting date")))
	       (date-max (replace-regexp-in-string "-" "" (org-read-date nil nil nil "Enter ending date"))))
	    (setq context (format "[%s0000+TO+%s0000]" date-min date-max))
	    (setq temp-query-info (concat temp-query-info "time:" (format "%s-%s" date-min date-max))))))
	(if (string-match "^ *$" context)
	    (message "Void search argument.")
	  (setq arxiv-query-info (concat arxiv-query-info temp-query-info))
	  (setq arxiv-query-data-list (cons (list field condition context) arxiv-query-data-list)))) ; this reversed the order of the list, need to fix it later on
    (message "Only inclusive searching is allowed as the first keyword."))
  (arxiv-search-menu/body))

(defun arxiv-hydra-perform-search ()
  "helper function for arxiv-search-menu()."
  (interactive)
  (if arxiv-query-data-list
      (progn
	(setq arxiv-query-info (replace-regexp-in-string "^+" "" arxiv-query-info))
	(setq arxiv-query-data-list (nreverse arxiv-query-data-list)) ; fix the reverse order caused in qrxiv-query-data-update ()
	(setq arxiv-entry-list (arxiv-query-general))
	(setq arxiv-mode-entry-function 'arxiv-complex-search)
	(arxiv-populate-page 0 arxiv-entries-per-page))
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
  ("q" (setq arxiv-query-data-list nil) "quit")
  )
 
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
  ("q" (setq arxiv-query-data-list nil) "quit")
  )

(defhydra arxiv-help-menu (:color blue :forien-keys run)
  "
ArXiv mode help message
---------------------------------------------------------------------------------------------------------
_n_: next entry           _SPC_: toggle abstract window          _b_: export bibtex entry
_p_: previous entry       _RET_: open link in browser            _e_: download pdf & export bibtex entry
_r_: refine search          _d_: download PDF                    _?_: toggle this help
_q_: quit Arxiv mode        
"
  ("n" arxiv-next-entry)
  ("p" arxiv-prev-entry)
  ("r" arxiv-refine-search :exit t)
  ("q" arxiv-exit :exit t)
  ("SPC" arxiv-show-hide-abstract)
  ("RET" arxiv-open-current-url)
  ("d" arxiv-download-pdf)
  ("?" nil)
  ("b" arxiv-export-bibtex)
  ("e" arxiv-download-pdf-export-bibtex)
  )

(provide 'arxiv-mode)

;;; arxiv-mode.el ends here

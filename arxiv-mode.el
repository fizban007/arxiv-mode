;; Defines the arxiv-mode
;; 
;; Author: Alex Chen (fizban007)
;; Email: fizban007 (at) gmail (dot) com
;;
;; Modified by Simon Lin (Simon-Lin)
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

(defun arxiv-next-entry (&optional arg)
  "Move to the next arXiv entry"
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
  "Move to the previous arXiv entry"
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
  (when arxiv-abstract-window
    (arxiv-show-abstract)))

(defun arxiv-select-entry ()
    "Select the entry to which the cursor is pointing to"
    (interactive)
    (setq arxiv-current-entry (/ (line-number-at-pos) 4))
    (goto-char (point-min))
    (forward-line (* 4 arxiv-current-entry))
    (move-overlay arxiv-highlight-overlay
		  (point) (progn (beginning-of-line 5) (point)))
    (forward-line (- 4))
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
    (setq header-line-format (format " arXiv:%s" (cdr (assoc 'id (nth arxiv-current-entry arxiv-entry-list)))))
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

(defun arxiv-SPC ()
  "If the cursor position does not correspond to the current entry, 
  move the current entry to the corresponding position. Otherwise call
  arxiv-show-hide-abstract."
  (interactive)
  (if (eq (/ (line-number-at-pos) 4) arxiv-current-entry)
      (arxiv-show-hide-abstract)
    (arxiv-select-entry)))

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
(define-key arxiv-mode-map (kbd "SPC") 'arxiv-SPC)
(define-key arxiv-mode-map "d" 'arxiv-download-pdf)
(define-key arxiv-mode-map "e" 'arxiv-download-pdf-export-bibtex)
(define-key arxiv-mode-map "b" 'arxiv-export-bibtex)
(define-key arxiv-mode-map "r" 'arxiv-refine-search)
(define-key arxiv-mode-map "q" 'arxiv-exit)
(define-key arxiv-mode-map (kbd "?") 'arxiv-help-menu/body)

(defun arxiv-mode ()
  "Major mode for reading arXiv updates online.
Press ? for a list of availble commands."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'arxiv-mode)
  (setq mode-name "arXiv")
  (use-local-map arxiv-mode-map)
  (setq header-line-format '(:eval (arxiv-headerline-format)))
  (setq arxiv-highlight-overlay (make-overlay 1 1))
  (overlay-put arxiv-highlight-overlay 'face 'highlight)
  (run-mode-hooks 'arxiv-mode-hook))

(defun arxiv-headerline-format ()
  "update the header line of *arxiv-update* buffer."
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
  "Fill (insert) the details of the article list according to arxiv-entry-list.
If min-entry and max-entry are ignored, defaults to fill with the whole arxiv-entry-list."
  (unless min-entry
    (setq min-entry 0))
  (let ((arxiv-entry-list-trun (seq-subseq arxiv-entry-list min-entry max-entry))) ; if max is omitted it defaults to be len(list)
    (mapcar
     (lambda (entry)
       (progn
	 (arxiv-insert-with-face (format  " %s\n " (alist-get 'title entry)) 'arxiv-title-face)
	 (let ((authors (alist-get 'authors entry)))
	   (while authors
	     (progn 
	       (arxiv-insert-with-face (format "%s" (car authors)) 'arxiv-author-face)
	       (setq authors (cdr authors))
	       (if authors
		   (arxiv-insert-with-face ", " 'arxiv-author-face))
	       )))
	 (let ((date (alist-get 'date entry)))
	   (string-match "^[-[:digit:]]+ " date)
	   (arxiv-insert-with-face (format "\n %s " (match-string 0 date)) 'arxiv-date-face))
	 (let ((cats (alist-get 'categories entry)))
	   (dolist (cat cats)
	     (arxiv-insert-with-face (format "[%s] " cat) 'arxiv-keyword-face)))
	 (insert "\n\n")))
     arxiv-entry-list-trun)))

(defun arxiv-populate-page (&optional arxiv-buffer)
  "Populate the page of results according to arxiv-entry-list."
  (if arxiv-entry-list
      (progn
	(unless arxiv-buffer
	  (setq arxiv-buffer (get-buffer-create "*arXiv-update*")))
	(save-excursion
	  (set-buffer arxiv-buffer)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (arxiv-fill-page)
	  (goto-char (point-min))
	  (setq arxiv-current-entry 0)
	  (arxiv-mode)
	  (move-overlay arxiv-highlight-overlay
			(point) (progn (beginning-of-line 5) (point)))
	  (goto-char (point-min))
	  (message "Showing results %d-%d of %d" arxiv-query-results-min arxiv-query-results-max arxiv-query-total-results)
	  (setq buffer-read-only t))
	(switch-to-buffer arxiv-buffer))
    (message "No articles matching the search condition.")))

(defun arxiv-show-next-page (&optional arxiv-buffer)
  "Perform one more query (according to arxiv-current-entry and arxiv-entries-per-page) and fill the results into buffer."
  (unless arxiv-buffer
    (setq arxiv-buffer (get-buffer "*arXiv-update*")))
  (let* ((min (* arxiv-entries-per-page (/ (+ 1 arxiv-current-entry) arxiv-entries-per-page)))
	 (max (+ min arxiv-entries-per-page)))
    (when (> max arxiv-query-total-results)
      (setq max arxiv-query-total-results))
    (message "Fetching results %d-%d..." (+ 1 min) max)
    (cond
     ((or (eq arxiv-mode-entry-function 'arxiv-read-new) (eq arxiv-mode-entry-function 'arxiv-read-recent))
      (setq arxiv-entry-list
	    (append arxiv-entry-list
		    (arxiv-query (alist-get 'category arxiv-query-data-list)
				 (alist-get 'date-start arxiv-query-data-list)
				 (alist-get 'date-end arxiv-query-data-list)				 
				 min))))
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
      (bibtex-set-dialect 'BibTeX t)
      (setq key (bibtex-generate-autokey)))      
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
      ((time (current-time))
       (day (string-to-number (format-time-string "%u" time "EST")))       
       (date-start nil)
       (date-end nil)
       (dayname-start "")
       (dayname-end "")
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
    ;; arxiv announces new submssions on 20:00 EST. Check it's 20:00 yet.
    (when (< (string-to-number (format-time-string "%H" time "EST")) 20)
      (setq day (- day 1))
      (setq time (time-subtract time 86400)))
    
    (cond
     ((equal day 5) ; Friday
      (progn
	(setq date-start (format-time-string "%Y%m%d" (time-subtract time (* 2 86400)) "EST"))
	(setq date-end (format-time-string "%Y%m%d" (time-subtract time 86400) "EST"))
	(setq dayname-start "Wed")
	(setq dayname-end "Thu")))
     ((equal day 6) ; Saturday
      (progn
	(setq date-start (format-time-string "%Y%m%d" (time-subtract time (* 3 86400)) "EST"))
	(setq date-end (format-time-string "%Y%m%d" (time-subtract time (* 2 86400)) "EST"))
	(setq dayname-start "Wed")
	(setq dayname-end "Thu")))
     ((or (equal day 7) (eq day 0)) ; Sunday
      (progn
	(setq date-start (format-time-string "%Y%m%d" (time-subtract time (* 3 86400)) "EST"))
	(setq date-end (format-time-string "%Y%m%d" (time-subtract time (* 2 86400)) "EST"))
	(setq dayname-start "Thu")
	(setq dayname-end "Fri")))
     ((equal day 1) ; Monday
      (progn
	(setq date-start (format-time-string "%Y%m%d" (time-subtract time (* 3 86400)) "EST"))
	(setq date-end (format-time-string "%Y%m%d" time "EST"))
	(setq dayname-start "Fri")
	(setq dayname-end "Mon")))
     (t ; Tue - Thu, read from previous day
      (progn
	(setq date-start (format-time-string "%Y%m%d" (time-subtract time 86400) "EST"))
	(setq date-end (format-time-string "%Y%m%d" time "EST"))
	(setq dayname-start (format-time-string "%a" (time-subtract time 86400) "EST"))
	(setq dayname-end (format-time-string "%a" time "EST")))))
    ;; day to week name
    (setq arxiv-query-info (format " Showing new submissions in %s from %s(%s) to %s(%s) (EST)."
				   category date-start dayname-start date-end dayname-end))
    (setq date-start (concat date-start "1400"))
    (setq date-end (concat date-end "1400"))
    (setq arxiv-entry-list (arxiv-query category date-start date-end))
    (setq arxiv-query-data-list `((date-start . ,date-start) (date-end . ,date-end) (category . ,category)))
    (setq arxiv-mode-entry-function 'arxiv-read-new)
    (arxiv-populate-page)))

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
    (setq arxiv-query-data-list `((date-start . ,date-start) (date-end . ,date-end) (category . ,category)))
    (setq arxiv-mode-entry-function 'arxiv-read-recent)
    (arxiv-populate-page)))

(defun arxiv-read-author ()
  "Find the papers by author name."
  (interactive)
  (let*
      ((author (read-string "Authors name (use space to seperate): "))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
    (setq arxiv-entry-list (arxiv-query-author author category))
    (setq arxiv-query-info (format " Showing results for author(s): %s in categroy %s." author category))
    (setq arxiv-query-data-list `((author . ,author) (category . ,category)))
    (setq arxiv-mode-entry-function 'arxiv-read-author)
    (arxiv-populate-page)))

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

(defhydra arxiv-help-menu (:color red :foriegn-keys run)
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
  ("SPC" arxiv-SPC)
  ("RET" arxiv-open-current-url)
  ("d" arxiv-download-pdf)
  ("?" nil)
  ("b" arxiv-export-bibtex)
  ("e" arxiv-download-pdf-export-bibtex)
  )

(provide 'arxiv-mode)

;;; arxiv-mode.el ends here

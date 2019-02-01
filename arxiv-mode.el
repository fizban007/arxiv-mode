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
  (arxiv-update-headerline)
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
  (arxiv-update-headerline)
  (when arxiv-abstract-window
    (arxiv-show-abstract)))

(defun arxiv-open-current-url ()
  "Open the webpage for the current highlighted paper entry."
  (interactive)
  ;; (message "%S" (nth arxiv-current-entry arxiv-entry-list))
  (setq url (cdr (assoc 'url (nth arxiv-current-entry arxiv-entry-list))))
  ;; (start-process "arxiv-webpage" nil arxiv-default-browser url)
  (browse-url url))

(defun arxiv-download-pdf ()
  "Download and save the highlighted paper to desired folder.
You can change the default folder by customizing the variable arxiv-default-download-folder."
  (interactive)
  (let ((url (cdr (assoc 'pdf (nth arxiv-current-entry arxiv-entry-list))))
	(newfile nil))
    (string-match "/[^/]+?$" url)
    (setq newfile (concat (match-string 0 url) ".pdf"))
    (setq newfile (read-file-name "save pdf as: "
				  (expand-file-name arxiv-default-download-folder)
				  (concat arxiv-default-download-folder newfile)
				  nil newfile))
    (url-copy-file url newfile)))

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
;; (define-key arxiv-mode-map "q" '(lambda () (interactive) (kill-buffer "*arXiv-update*")))
(define-key arxiv-mode-map "q" 'arxiv-exit)

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
  (setq header-line-format "")
  ;; (setq font-lock-multiline t)
  (run-mode-hooks 'arxiv-mode-hook))

  ;; (eval-after-load 'evil
  ;;   (evil-emacs-state)))

(defun arxiv-update-headerline ()
  "update the header line of *arxiv-update* buffer."
  (setq header-line-format (format " search result for: %s  %d/%d" arxiv-api-url (+ 1 arxiv-current-entry) arxiv-query-total-results)))

(defun arxiv-populate-page (page num-per-page &optional arxiv-buffer)
  "Populate the page of results according to arxiv-entry-list."
  (if arxiv-entry-list
      (progn
	(unless arxiv-buffer
	  (setq arxiv-buffer (get-buffer-create "*arXiv-update*")))
	(save-excursion
	  (set-buffer arxiv-buffer)
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
	  (arxiv-update-headerline)
	  (setq buffer-read-only t))
	(switch-to-buffer arxiv-buffer))
    (message "No articles at this time.")))

(defun arxiv-read ()
  "read arXiv articles published on a given date, in a specific category."
  (interactive)
  (let*
      ((date (string-to-number (replace-regexp-in-string "-" "" (org-read-date nil nil nil "Enter desired date"))))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
    (setq arxiv-entry-list (arxiv-query category (int-to-string date) (int-to-string (+ 1 date))))
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-read-new ()
  "read new (submitted in the previous work day) arXiv articles in a given category."
  (interactive)
  (let*
      ((date (string-to-number (format-time-string "%Y%m%d")))
       (day (string-to-number (format-time-string "%u")))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))       
    (cond ((eq day 7) (setq date (- date 2)))
	  ((eq day 1) (setq date (- date 3)))
	  (t (setq date (- date 1))))
    (setq arxiv-entry-list (arxiv-query category (int-to-string date) (int-to-string (+ 1 date))))
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-read-recent ()
  "read recent (past week) submissions of arXiv in a given category."
  (interactive)
  (let*
      ((date-end (string-to-number (format-time-string "%Y%m%d")))
       (date-start (- date-end 7))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))       
    (setq arxiv-entry-list (arxiv-query category (int-to-string date-start) (int-to-string date-end)))
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-read-author ()
  "Find the papers by author name."
  (interactive)
  (let*
      ((author (read-string "Authors name (use space to seperate): "))
       (category (completing-read "Select category: "
				  arxiv-categories nil t nil nil arxiv-default-category)))
  (setq arxiv-entry-list (arxiv-query-author author category))
  (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-complex-search ()
  "Do a complex search on arXiv database and list the result in buffer."
  (interactive)
  (setq arxiv-query-data-list nil)
  (arxiv-search-menu/body))

(defun arxiv-query-data-update (field condition)
  "Ask and update the variable arxiv-query-data-list in the corresponding search field.
Do exclusive update if condition is nil."
  (if (or condition arxiv-query-data-list)
      (let ((context))
	(cond
	 ((eq field 'all) (setq context (read-string "Search all fields (use space to seperate and \"\" to quote): ")))
	 ((eq field 'id) (setq context (read-string "Article ID: ")))
	 ((eq field 'author) (setq context (read-string "Authors name (use space to seperate): ")))
	 ((eq field 'abstract) (setq context (read-string "Abstract keywords (use space to seperate and \"\" to quote): ")))
	 ((eq field 'category) (setq context (completing-read "Category: "
							      arxiv-categories nil t nil nil arxiv-default-category)))
	 ((eq field 'journal) (setq context (read-string "Journal: ")))
	 ((eq field 'comment) (setq context (read-string "Search comments (use space to seperate and \"\" to quote): ")))
	 ((eq field 'time) (let
			       ((date-min (string-to-number (replace-regexp-in-string "-" "" (org-read-date nil nil nil "Enter starting date"))))
				(date-max (string-to-number (replace-regexp-in-string "-" "" (org-read-date nil nil nil "Enter ending date")))))
			     (setq context (format "[%d0000+TO+%d0000]" date-min date-max)))))
	(setq arxiv-query-data-list (cons (list field condition context) arxiv-query-data-list)))
    (message "Only inclusive searching is allowed as the first keyword."))
  (arxiv-search-menu/body))

(defun arxiv-hydra-quit-function ()
  "helper function for arxiv-search-menu()."
  (interactive)
  (if arxiv-query-data-list
      (progn
	(setq arxiv-query-data-list (nreverse arxiv-query-data-list))
	(setq arxiv-entry-list (arxiv-query-general))
	(arxiv-populate-page 0 arxiv-entries-per-page))
    (message "quit without search conditions")))


(defhydra arxiv-search-menu (:color blue :foreign-keys warn :exit t)
  "
Add arXiv search condition:
_a_: all                   _i_: article ID            _t_: submitted time
_u_: author(s)             _b_: abstract              _c_: category
_j_: journal               _m_: comment               _-_: exclude condition 
_p_: perform search with current condition(s)       _q_: quit
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
  ("p" arxiv-hydra-quit-function)
  ("q" (setq arxiv-query-data-list nil) "quit")
  )
 
(defhydra arxiv-search-menu-ex (:color red :foreign-keys warn :exit t)
  "
Exclude arXiv search condition:
_a_: all                   _i_: article ID            _t_: submitted time
_u_: author(s)             _b_: abstract              _c_: category
_j_: journal               _m_: comment               _+_: include condition 
_p_: perform search with current condition(s)       _q_: quit
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
  ("p" arxiv-hydra-quit-function)
  ("q" (setq arxiv-query-data-list nil) "quit")
  )

(provide 'arxiv-mode)

;;; arxiv-mode.el ends here

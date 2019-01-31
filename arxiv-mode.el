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
  ;; (message "%S" (nth arxiv-current-entry arxiv-entry-list))
  (setq url (cdr (assoc 'url (nth arxiv-current-entry arxiv-entry-list))))
  ;; (start-process "arxiv-webpage" nil arxiv-default-browser url)
  (browse-url url)
  )

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
  (kill-buffer "*arXiv-update*")
  (if (get-buffer "*arXiv-abstract*")
      (kill-buffer "*arXiv-abstract*"))
  (when arxiv-abstract-window
    (delete-window arxiv-abstract-window)
    (setq arxiv-abstract-window nil)))


;;======================convinent keymaps for iserlohn========================================
(setq arxiv-mode-map (make-sparse-keymap))
(define-key arxiv-mode-map "k" 'arxiv-next-entry)
(define-key arxiv-mode-map "i" 'arxiv-prev-entry)
(define-key arxiv-mode-map (kbd "RET") 'arxiv-open-current-url)
(define-key arxiv-mode-map (kbd "SPC") 'arxiv-show-hide-abstract)
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
  ;; (setq font-lock-multiline t)
  (run-mode-hooks 'arxiv-mode-hook))
  ;; (eval-after-load 'evil
  ;;   (evil-emacs-state)))

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
	  ;; (message "Total number of entries: %d" (safe-length arxiv-entry-list))
	  (message "Total number of entries: %d" arxiv-query-total-results)
	  (setq buffer-read-only t))
	(switch-to-buffer arxiv-buffer))
    ((message "No articles at this time."))))

(defun arxiv-read ()
  "read arXiv articles published on a given date, in a specific category."
  (interactive)
  (let*
      ((date (string-to-number (replace-regexp-in-string "-" "" (org-read-date nil nil nil "Enter desired date"))))
       (category (completing-read "Select catagory: "
				  arxiv-catagories nil t nil nil arxiv-default-catagory)))
    (setq arxiv-entry-list (arxiv-query category (int-to-string date) (int-to-string (+ 1 date))))
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-read-new ()
  "read new (submitted in the previous work day) arXiv articles in a given category."
  (interactive)
  (let*
      ((date (string-to-number (format-time-string "%Y%m%d")))
       (day (string-to-number (format-time-string "%u")))
       (category (completing-read "Select catagory: "
				  arxiv-catagories nil t nil nil arxiv-default-catagory)))       
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
       (category (completing-read "Select catagory: "
				  arxiv-catagories nil t nil nil arxiv-default-catagory)))       
    (setq arxiv-entry-list (arxiv-query category (int-to-string date-start) (int-to-string date-end)))
    (arxiv-populate-page 0 arxiv-entries-per-page)))

(defun arxiv-read-author ()
  "Find the papers by author name."
  (interactive)
  (let*
      ((author (read-string "Authors name (use space to seperate): "))
       (category (completing-read "Select catagory: "
				  arxiv-catagories nil t nil nil arxiv-default-catagory)))
  (setq arxiv-entry-list (arxiv-query-author author category))
  (arxiv-populate-page 0 arxiv-entries-per-page)))

(provide 'arxiv-mode)

;;; arxiv-mode.el ends here

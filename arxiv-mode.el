;; Defines the arxiv-mode
(require 'overlay)
(require 'button)
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
  (start-process "arxiv-webpage" nil "firefox" url))

(defun arxiv-show-abstract (&optional arg)
  "Show the abstract for the current highlighted entry."
  (interactive)
  (unless arxiv-abstract-window
    (setq abstract-window (split-window-right)))
  (setq abstract-buffer (get-buffer-create "*arXiv-abstract*"))
  (with-selected-window abstract-window
    (switch-to-buffer abstract-buffer)
    (set-buffer abstract-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((entry (nth arxiv-current-entry arxiv-entry-list)))
      (progn 
        (insert (format "Title: %s\n\nAuthors: " (cdr (assoc 'title entry))))
        (let ((authors (cdr (assoc 'authors entry))))
          (while authors
            (progn 
              (insert (format "%s" (car authors)))
              (setq authors (cdr authors))
              (if authors
                  (insert ", "))
              )))
        (insert (format "\n\nSubmitted: %s" (cdr (assoc 'date entry))))
        (insert (format "\n\nAbstract: %s" (cdr (assoc 'abstract entry))))
        ))
    (arxiv-abstract-mode)
    (setq buffer-read-only t)
    )
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

(setq arxiv-mode-map (make-sparse-keymap))
(define-key arxiv-mode-map "n" 'arxiv-next-entry)
(define-key arxiv-mode-map "p" 'arxiv-prev-entry)
(define-key arxiv-mode-map "u" 'arxiv-open-current-url)
(define-key arxiv-mode-map "a" 'arxiv-show-hide-abstract)
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
  (run-hooks 'arxiv-mode-hook))
  ;; (eval-after-load 'evil
  ;;   (evil-emacs-state)))

(defun arxiv-populate-page (page num-per-page date category &optional arxiv-buffer arg)
  "Populate the page of results according to provided search conditions."
  ;; TODO: Need to figure out page separation mechanism
  (interactive)
  (catch 'myTag 
    (setq arxiv-entry-list (arxiv-query-latest category date))
    (unless arxiv-entry-list
      (throw 'myTag "No articles at this time."))
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
           (insert (format "\nDate: %s\n\n" (cdr (assoc 'date entry))))
           ))
       arxiv-entry-list)
      (goto-char (point-min))
      (arxiv-highlight-entry 0 (point))
      (setq arxiv-current-entry 0)
      (arxiv-mode)
      ;; (message "Total number of entries: %d" (safe-length arxiv-entry-list))
      (message "Total number of entries: %d" arxiv-query-total-results)
      (setq buffer-read-only t))
    (switch-to-buffer arxiv-buffer)
    arxiv-buffer))
    
    

(defun arxiv-read (date category)
  "Read the articles submitted the day before a given date, in a
given category. Defaults to today."
  (interactive (list (read-string "Enter desired date (default today): ") (read-string "Enter desired category (default astro-ph): ")))
  (if (equal date "")
    (setq date (format-time-string "%Y%m%d")))
  (if (equal category "")
    (setq category "astro-ph"))
  ;; (message "%S %S" date category)
  (arxiv-populate-page 0 arxiv-entries-per-page date category)
)

;; (arxiv-read-latest "20131008")
     ;; (insert "\n

(provide 'arxiv-mode)

;;; arxiv-mode.el ends here

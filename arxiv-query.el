;; arXiv query functions

(require 'xml)

; URL of the arXiv api
(setq arxiv-url "http://export.arxiv.org/api/query")
(setq arxiv-query-total-results nil)
;; (setq arxiv-search-parameter "?search_query=submittedDate:[20130910+TO+20130920]+AND+cat:astro-ph*&sortBy=submittedDate&sortOrder=descending&start=0&max_results=30")
;; (message "%s" arxiv-url)

;; Function for extracting the url for pdf file recursively
(defun arxiv-extract-pdf (my-list)
  (if my-list
      (let* ((sub-list (car (cdr (car my-list))))
             (sub-title (cdr (assoc 'title sub-list))))
        ;; (message "%S\n :%S" sub-list sub-title)
        (if (and sub-title (equal sub-title "pdf"))
            (progn 
              ;; (message sub-title)
              (cdr (assoc 'href sub-list)))
          (arxiv-extract-pdf (cdr my-list))))))

;; Search the arXiv for articles between dateStart and dateEnd in the specified category
(defun arxiv-search-date (dateStart dateEnd category &optional start max-num)
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num 50))  ; Default to 50 results per page
  (format "%s?search_query=submittedDate:[%s0000+TO+%s0000]+AND+cat:%s*&sortBy=submittedDate&sortOrder=descending&start=%d&max_results=%d" 
          arxiv-url dateStart dateEnd category start max-num))

(defun arxiv-search-author (author category &optional start max-num)
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num 50))  ; Default to 50 results per page
  (format "%s?search_query=au:%s+AND+cat:%s*&start=%d&max_results=%d"
          arxiv-url author category start max-num))

(defun arxiv-query (url)
  (catch 'myTag
    ;; (setq date-start date)
    ;; (setq date-end (number-to-string (+ (string-to-int date) 1)))
    ;; ;; (setq date-end date)
    ;; ;; (setq date-start (number-to-string (+ (string-to-int date) -1)))
    (setq my-list nil)
    (setq my-buffer (url-retrieve-synchronously url))
    ;; (message "%s" my-buffer)
    (set-buffer my-buffer)
    (goto-char (point-min))
    ;; (message "%d" (point-max))
    (setq my-point (search-forward "<?xml"))
    (goto-char (- my-point 5))
    (setq root (libxml-parse-xml-region (point) (point-max)))
    (setq arxiv-query-total-results (string-to-int (nth 2 (car (xml-get-children root 'totalResults)))))
    (message "%S" arxiv-query-total-results)
    (setq entries (xml-get-children root 'entry))
    (unless entries
      (throw 'myTag nil))
    ;; (message "%d" (safe-length entries))
    ;; (message "%s" (nth 0 entries))
    (mapcar 
     (lambda (entry) 
       (progn
         ;; (message "a" )
         ;; (message "%s" entry)
         (setq paper (xml-node-children entry))
         ;; (message "%S" (xml-get-children paper 'link))
         ;; (message "%S" (arxiv-extract-pdf (xml-get-children paper 'link)))
         (setq my-pdf (arxiv-extract-pdf (xml-get-children paper 'link)))
         ;; (message "%S" (cdr (assoc 'id paper)))
         ;; (message "%S" (car (xml-node-children (car paper))))
         (setq my-url (nth 1 (cdr (assoc 'id paper))))
         ;; (message "%S" my-url)
         (setq my-title (car (last (xml-node-children (car (xml-get-children paper 'title))))))
         ;; (message title)
         (setq my-title (replace-regexp-in-string "[ \n]+" " " my-title))
         (setq my-abstract (car (xml-node-children (car (xml-get-children paper 'summary)))))
         (setq my-publishdate (car (xml-node-children (car (xml-get-children paper 'published)))))
         (setq my-publishdate (replace-regexp-in-string "[TZ]" " " my-publishdate))
         (setq my-authors (xml-get-children paper 'author))
         (setq my-names (mapcar 
                      (lambda (author) (car (last (car (xml-get-children author 'name)))))
                      my-authors))
         (setq alist-entry `((title . ,my-title)
                             (authors . ,my-names)
                             (abstract . ,my-abstract)
                             (url . ,my-url)
                             (date . ,my-publishdate)
                             (pdf . ,my-pdf)))
         (add-to-list 'my-list alist-entry)
         (setq my-list (cl-sort my-list #'string-greaterp :key (lambda (entry) (cdr (assoc 'date entry)))))
         ;; (message "%S\n" alist-entry)
         ;; )) entries)
         )) entries)
    my-list))

;; Query the arXiv for the articles between date and date+1
(defun arxiv-query-latest (cat &optional date start max-num)
  (catch 'myTag
    (setq date-start date)
    (setq date-end (number-to-string (+ (string-to-int date) 1)))
    ;; (setq date-end date)
    ;; (setq date-start (number-to-string (+ (string-to-int date) -1)))
    (setq my-list nil)
    (setq my-buffer (url-retrieve-synchronously (arxiv-search-date date-start date-end cat)))
    ;; (message "%s" my-buffer)
    (set-buffer my-buffer)
    (goto-char (point-min))
    ;; (message "%d" (point-max))
    (setq my-point (search-forward "<?xml"))
    (goto-char (- my-point 5))
    (setq root (libxml-parse-xml-region (point) (point-max)))
    (setq arxiv-query-total-results (string-to-int (nth 2 (car (xml-get-children root 'totalResults)))))
    (message "%S" arxiv-query-total-results)
    (setq entries (xml-get-children root 'entry))
    (unless entries
      (throw 'myTag nil))
    ;; (message "%d" (safe-length entries))
    ;; (message "%s" (nth 0 entries))
    (mapcar 
     (lambda (entry) 
       (progn
         ;; (message "a" )
         ;; (message "%s" entry)
         (setq paper (xml-node-children entry))
         ;; (message "%S" (xml-get-children paper 'link))
         ;; (message "%S" (arxiv-extract-pdf (xml-get-children paper 'link)))
         (setq my-pdf (arxiv-extract-pdf (xml-get-children paper 'link)))
         ;; (message "%S" (cdr (assoc 'id paper)))
         ;; (message "%S" (car (xml-node-children (car paper))))
         (setq my-url (nth 1 (cdr (assoc 'id paper))))
         ;; (message "%S" my-url)
         (setq my-title (car (last (xml-node-children (car (xml-get-children paper 'title))))))
         ;; (message title)
         (setq my-title (replace-regexp-in-string "[ \n]+" " " my-title))
         (setq my-abstract (car (xml-node-children (car (xml-get-children paper 'summary)))))
         (setq my-publishdate (car (xml-node-children (car (xml-get-children paper 'published)))))
         (setq my-publishdate (replace-regexp-in-string "[TZ]" " " my-publishdate))
         (setq my-authors (xml-get-children paper 'author))
         (setq my-names (mapcar 
                      (lambda (author) (car (last (car (xml-get-children author 'name)))))
                      my-authors))
         (setq alist-entry `((title . ,my-title)
                             (authors . ,my-names)
                             (abstract . ,my-abstract)
                             (url . ,my-url)
                             (date . ,my-publishdate)
                             (pdf . ,my-pdf)))
         ;; (add-to-list 'my-list alist-entry)
         (setq my-list (append my-list alist-entry))
         ;; (message "%S\n" alist-entry)
         ;; )) entries)
         )) entries)
    my-list))
    ;; (message "%s" root)
    ;; (message "\n Title: %s\n Authors: %S\n URL: %s\n Abstract: %S \n"
    ;;          title names url abstract)
  
(provide 'arxiv-query)
;;; arxiv-query.el ends hereG

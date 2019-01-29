;; arXiv query functions

(require 'xml)

;; URL of the arXiv api
;; check https://arxiv.org/help/api/user-manual for information
(setq arxiv-url "http://export.arxiv.org/api/query")
(setq arxiv-query-total-results nil)

(defun arxiv-extract-pdf (my-list)
  "Function for extracting the url for pdf file recursively"
  (if my-list
      (let* ((sub-list (car (cdr (car my-list))))
             (sub-title (cdr (assoc 'title sub-list))))
        ;; (message "%S\n :%S" sub-list sub-title)
        (if (and sub-title (equal sub-title "pdf"))
            (progn 
              ;; (message sub-title)
              (cdr (assoc 'href sub-list)))
          (arxiv-extract-pdf (cdr my-list))))))

(defun arxiv-geturl-date (dateStart dateEnd category &optional start max-num)
  "get the API url for articles between dateStart and dateEnd in the specified category."
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num 100))  ; Default to 100 results per page
  (format "%s?search_query=submittedDate:[%s0000+TO+%s0000]+AND+cat:%s*&sortBy=submittedDate&sortOrder=descending&start=%d&max_results=%d" 
          arxiv-url dateStart dateEnd category start max-num))

(defun arxiv-geturl-author (author &optional category start max-num)
  "get the API url for articles by certain author."
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num 100))  ; Default to 100 results per page
  (setq author (replace-regexp-in-string " " "+" author))
  (setq author (replace-regexp-in-string "\"" "%22" author))
  (if category
      (format "%s?search_query=au:%s+AND+cat:%s*&start=%d&max_results=%d"
	      arxiv-url author category start max-num)
    (format "%s?search_query=au:%s&start=%d&max_results=%d"
	      arxiv-url author start max-num)))  

(defun arxiv-getxml-context (node child-name)
  "wrapper to get the context of the node directly."
  (car (xml-node-children (car (xml-get-children node child-name)))))

(defun arxiv-parse-api (url)
  "Call arXiv api url and parse its response.
Return a alist with various fields."
  (let ((my-list) (my-buffer)))
  (setq my-list nil)
  (setq my-buffer (url-retrieve-synchronously url))
  (set-buffer my-buffer)
  ;; (goto-char (point-min))
  ;;(message "%d" (point-max))
  ;;(setq my-point (search-forward "<?xml"))
  ;;(goto-char (- my-point 5))
  (setq root (car (xml-parse-region)))
  (setq arxiv-query-total-results (string-to-number (arxiv-getxml-context root 'opensearch:totalResults)))
  (message "%S" arxiv-query-total-results)
  (setq entries (xml-get-children root 'entry))
  (mapcar 
   (lambda (paper)
     (progn
       (setq my-pdf (arxiv-extract-pdf (xml-get-children paper 'link)))
       (setq my-url (arxiv-getxml-context paper 'id))
       (setq my-title (arxiv-getxml-context paper 'title))
       (setq my-title (replace-regexp-in-string "[ \n]+" " " my-title))
       (setq my-abstract (arxiv-getxml-context paper 'summary))
       (setq my-abstract (replace-regexp-in-string "\n" " " my-abstract))
       (setq my-publishdate (arxiv-getxml-context paper 'published))
       (setq my-publishdate (replace-regexp-in-string "[TZ]" " " my-publishdate))
       (setq my-updatedate (arxiv-getxml-context paper 'updated))
       (setq my-updatedate (replace-regexp-in-string "[TZ]" " " my-updatedate))
       (setq my-authors (xml-get-children paper 'author))
       (setq my-names (mapcar (lambda (author) (arxiv-getxml-context author 'name)) my-authors))
       (setq my-doi (arxiv-getxml-context paper 'arxiv:doi))
       (setq my-comment (arxiv-getxml-context paper 'arxiv:comment))
       (setq my-journal (arxiv-getxml-context paper 'arxiv:journal_ref))
       (setq my-categories (xml-get-children paper 'category))
       (setq my-categories (mapcar (lambda (cat) (cdr (assq 'term (xml-node-attributes cat)))) my-categories))
       (setq alist-entry `((title . ,my-title)
			   (authors . ,my-names)
			   (abstract . ,my-abstract)
			   (url . ,my-url)
			   (date . ,my-publishdate)
			   (updated . ,my-updatedate)
			   (doi . ,my-doi)
			   (comment . ,my-comment)			   
			   (journal . ,my-journal)
			   (categories . ,my-categories)
			   (pdf . ,my-pdf)))
       (setq my-list (append my-list `(,alist-entry)))
       ;; (message "%S\n" alist-entry)
       ;; )) entries)
       )) entries)
  my-list)
;; (message "\n Title: %s\n Authors: %S\n URL: %s\n Abstract: %S \n"
;;          title names url abstract)

(defun arxiv-query (cat date-start date-end &optional max-num)
  "Query arXiv for articles in a given category submitted between date-start and date-end."
  (unless (> (string-to-number date-end) (string-to-number date-start))
    (user-error "incorrect date specification"))  
  (arxiv-parse-api (arxiv-geturl-date date-start date-end cat)))

(defun arxiv-query-author (author &optional cat max-num)
  "Query arXiv for articles by certain authors (in a given category)."
  (arxiv-parse-api (arxiv-geturl-author author cat)))
  
(provide 'arxiv-query)
;;; arxiv-query.el ends hereG

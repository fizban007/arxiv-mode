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

(defun arxiv-parse-query-data (query-string)
  "Helper function to parse the input search data to pre-api form.
replace space by + and \" to %22"
  (setq query-string (replace-regexp-in-string "\\(^ +\\| +$\\)" "" query-string))
  (setq query-string (replace-regexp-in-string " +" "+" query-string))    
  (setq query-string (replace-regexp-in-string "\"" "%22" query-string))
  query-string)

(defun arxiv-get-api-url (&optional start max-num)
  "get the API url according to the arxiv-query-data-list.
When using this function, make sure that the first item of the list has t condition."
  (unless start (setq start 0))
  (unless max-num (setq max-num arxiv-entries-per-page))
  (let ((url (format "%s?search_query=" arxiv-url))
	(parsed-query nil)
	(body nil))
    (dolist (query-data arxiv-query-data-list url)
      (if body
	  (if (nth 1 query-data)
	      (setq url (concat url "+AND+"))
	    (setq url (concat url "+ANDNOT+")))
	(setq body t))
      (let ((field (car query-data)))
	(cond
	 ((eq field 'all) (setq url (concat url "all:")))
	 ((eq field 'id) (setq url (concat url "id:")))
	 ((eq field 'time) (setq url (concat url "submittedDate:")))
	 ((eq field 'title) (setq url (concat url "ti:")))
	 ((eq field 'author) (setq url (concat url "au:")))
	 ((eq field 'abstract) (setq url (concat url "abs:")))
	 ((eq field 'comment) (setq url (concat url "co:")))
	 ((eq field 'journal) (setq url (concat url "jr:")))
	 ((eq field 'category) (setq url (concat url "cat:"))))
	(setq url (concat url (arxiv-parse-query-data (nth 2 query-data))))))
    (setq url (concat url (format "&start=%d&max_results=%d" start max-num)))))

(defun arxiv-geturl-date (dateStart dateEnd category &optional start max-num)
  "get the API url for articles between dateStart and dateEnd in the specified category."
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num arxiv-entries-per-page))
  (format "%s?search_query=submittedDate:[%s+TO+%s]+AND+cat:%s*&sortBy=submittedDate&sortOrder=descending&start=%d&max_results=%d" 
          arxiv-url dateStart dateEnd category start max-num))

(defun arxiv-geturl-author (author &optional category start max-num)
  "get the API url for articles by certain author."
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num arxiv-entries-per-page))
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
  (setq root (car (xml-parse-region)))
  (setq arxiv-query-total-results (string-to-number (arxiv-getxml-context root 'opensearch:totalResults)))
  (setq arxiv-query-results-min (+ 1 (string-to-number (arxiv-getxml-context root 'opensearch:startIndex))))
  (setq arxiv-query-results-max (+ arxiv-query-results-min -1 (string-to-number (arxiv-getxml-context root 'opensearch:itemsPerPage))))
  (when (< arxiv-query-total-results arxiv-query-results-max) (setq arxiv-query-results-max arxiv-query-total-results))
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
       )) entries)
  my-list)

(defun arxiv-query (cat date-start date-end &optional max-num)
  "Query arXiv for articles in a given category submitted between date-start and date-end."
  (unless (> (string-to-number date-end) (string-to-number date-start))
    (user-error "incorrect date specification"))
  (arxiv-parse-api (arxiv-geturl-date date-start date-end cat)))

(defun arxiv-query-author (author &optional cat max-num)
  "Query arXiv for articles by certain authors (in a given category)."
  (arxiv-parse-api (arxiv-geturl-author author cat)))

(defun arxiv-query-general ()
  "Do a complex search on arXiv for articles according to the list arxiv-query-data-list."
  (arxiv-parse-api (arxiv-get-api-url)))

(provide 'arxiv-query)
;;; arxiv-query.el ends here

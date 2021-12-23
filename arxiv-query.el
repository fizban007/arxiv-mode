;;; arxiv-query.el --- arXiv query functions  -*- lexical-binding: t; -*-

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
  (unless max-num (setq max-num arxiv-entries-per-fetch))
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

(defun arxiv-geturl-date (dateStart dateEnd category &optional start max-num ascending)
  "Get the API url for articles between dateStart and dateEnd in the specified category.
If ASCENDING is t then sort the list by ascending order instead of descending."
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num arxiv-entries-per-fetch))
  (if ascending
      (setq ascending "ascending")
    (setq ascending "descending"))
  (format "%s?search_query=submittedDate:[%s+TO+%s]+AND+cat:%s*&sortBy=submittedDate&sortOrder=%s&start=%d&max_results=%d" 
              arxiv-url dateStart dateEnd category ascending start max-num))

(defun arxiv-geturl-author (author &optional category start max-num)
  "get the API url for articles by certain author."
  (unless start
    (setq start 0))  ; Start with the first result
  (unless max-num
    (setq max-num arxiv-entries-per-fetch))
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
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t) ;; enable utf-8 decoding
    ;; (view-buffer-other-window (current-buffer))
    (let* ((root (car (xml-parse-region)))
	   (entries (xml-get-children root 'entry))
	   (formated-list) (alist-entry)
	   (pdf) (link) (id) (title) (abstract) (publishdate) (updatedate) (authors) (names) (doi) (comment) (journal) (categories))
      (condition-case exception
	  (progn
	    (setq arxiv-query-total-results (string-to-number (arxiv-getxml-context root 'opensearch:totalResults)))
	    (setq arxiv-query-results-min (+ 1 (string-to-number (arxiv-getxml-context root 'opensearch:startIndex))))
	    (setq arxiv-query-results-max (+ arxiv-query-results-min -1 (string-to-number (arxiv-getxml-context root 'opensearch:itemsPerPage))))
	    (when (< arxiv-query-total-results arxiv-query-results-max) (setq arxiv-query-results-max arxiv-query-total-results))
	    (mapcar 
	     (lambda (paper)
	       (progn
		 (setq pdf (arxiv-extract-pdf (xml-get-children paper 'link)))
		 (setq link (arxiv-getxml-context paper 'id))
		 (string-match "^http://arxiv\\.org/abs/\\(.+\\)$" link)
		 (setq id (match-string 1 link))
		 (setq title (arxiv-getxml-context paper 'title))
		 (setq title (replace-regexp-in-string "[ \n]+" " " title))
		 (setq abstract (arxiv-getxml-context paper 'summary))
		 (setq abstract (replace-regexp-in-string "\n" " " abstract))
		 (setq publishdate (arxiv-getxml-context paper 'published))
		 (setq publishdate (replace-regexp-in-string "[TZ]" " " publishdate))
		 (setq updatedate (arxiv-getxml-context paper 'updated))
		 (setq updatedate (replace-regexp-in-string "[TZ]" " " updatedate))
		 (setq authors (xml-get-children paper 'author))
		 (setq names (mapcar (lambda (author) (arxiv-getxml-context author 'name)) authors))
		 (setq doi (arxiv-getxml-context paper 'arxiv:doi))
		 (setq comment (arxiv-getxml-context paper 'arxiv:comment))
		 (setq journal (arxiv-getxml-context paper 'arxiv:journal_ref))
		 (setq categories (xml-get-children paper 'category))
		 (setq categories (mapcar (lambda (cat) (cdr (assq 'term (xml-node-attributes cat)))) categories))
		 (setq alist-entry `((title . ,title)
				     (authors . ,names)
				     (abstract . ,abstract)
				     (url . ,link)
				     (id . ,id)
				     (date . ,publishdate)
				     (updated . ,updatedate)
				     (doi . ,doi)
				     (comment . ,comment)			   
				     (journal . ,journal)
				     (categories . ,categories)
				     (pdf . ,pdf)))
		 (setq formated-list (append formated-list (list alist-entry)))
		 )) entries)
	    formated-list)
	('error (progn
		  (switch-to-buffer (get-buffer-create "*arxiv-debug*"))
		  (print root (get-buffer "*arxiv-debug*"))
		  (error "Cannot parse the API query result. Refer to the debug buffer for informations.")))))))

(defun arxiv-query (cat date-start date-end &optional start ascending)
  "Query arXiv for articles in a given category submitted between date-start and date-end.
If ASCENDING is t then sort the list by ascending order instead of descending."
  (unless (> (string-to-number date-end) (string-to-number date-start))
    (user-error "incorrect date specification"))
  (arxiv-parse-api (arxiv-geturl-date date-start date-end cat start nil ascending)))

(defun arxiv-query-author (author &optional cat start ascending)
  "Query arXiv for articles by certain authors (in a given category)."
  (arxiv-parse-api (arxiv-geturl-author author cat start)))

(defun arxiv-query-general (&optional start)
  "Do a complex search on arXiv for articles according to the list arxiv-query-data-list."
  (arxiv-parse-api (arxiv-get-api-url start)))

(provide 'arxiv-query)
;;; arxiv-query.el ends here

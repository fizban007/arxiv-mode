;;; arxiv-query.el --- arXiv query functions  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'xml)

;; URL of the arXiv api
;; check https://arxiv.org/help/api/user-manual for information
(setq arxiv-url "http://export.arxiv.org/api/query")
(setq arxiv-query-total-results nil)

(defun arxiv-extract-pdf (my-list)
  "Extract the url for pdf file recursively from MY-LIST."
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
  "Parse the input search data (QUERY-STRING) to pre-api form.
In particular, replace space by + and \" to %22"
  (setq query-string (replace-regexp-in-string "\\(^ +\\| +$\\)" "" query-string))
  (setq query-string (replace-regexp-in-string " +" "+" query-string))
  (setq query-string (replace-regexp-in-string "\"" "%22" query-string))
  query-string)

(defun arxiv-get-api-url (&optional start)
  "Get the API url according to the `arxiv-query-data-list'.
START specifies starting index (default 0).
When using this function, make sure that the first item of the list has t condition."
  (unless start (setq start 0))
  (let ((url (format "%s?search_query=" arxiv-url))
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
    (if arxiv-query-sorting
	(setq url (concat url (format "&sortBy=%s&sortOrder=%s"
				      (plist-get arxiv-query-sorting :sortby)
				      (plist-get arxiv-query-sorting :sortorder)))))
    (setq url (concat url (format "&start=%d&max_results=%d" start arxiv-entries-per-fetch)))))

(defun arxiv-geturl-date (date-start date-end category &optional start ascending)
  "Get the API url for articles between DATE-START and DATE-END in CATEGORY.
START specifies starting index (default 0).
If ASCENDING is t then sort the list by ascending order instead of descending."
  (unless start
    (setq start 0))  ; Start with the first result
  (if ascending
      (setq ascending "ascending")
    (setq ascending "descending"))
  (format "%s?search_query=submittedDate:[%s+TO+%s]+AND+cat:%s*&sortBy=submittedDate&sortOrder=%s&start=%d&max_results=%d"
              arxiv-url date-start date-end category ascending start arxiv-entries-per-fetch))

(defun arxiv-geturl-author (author &optional category start)
  "Get the API url for articles used by `arxiv-read-author'.
AUTHOR and CATEGORY specifies search fields.
START specifies starting index (default 0)."
  (unless start
    (setq start 0))  ; Start with the first result
  (setq author (replace-regexp-in-string " " "+" author))
  (setq author (replace-regexp-in-string "\"" "%22" author))
  (if category
      (format "%s?search_query=au:%s+AND+cat:%s*&start=%d&max_results=%d"
	      arxiv-url author category start arxiv-entries-per-fetch)
    (format "%s?search_query=au:%s&start=%d&max_results=%d"
	      arxiv-url author start arxiv-entries-per-fetch)))

(defun arxiv-getxml-context (node child-name)
  "XML helper to get the context of CHILD-NAME from NODE directly."
  (car (xml-node-children (car (xml-get-children node child-name)))))

(defun arxiv-parse-api (url)
  "Call arXiv api (at URL) and parse its response.
Return a alist with various fields."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t) ;; enable utf-8 decoding
    ;; (view-buffer-other-window (current-buffer))
    (let* ((root (car (xml-parse-region)))
	   (entries (xml-get-children root 'entry))
	   (formated-list) (alist-entry)
	   (pdf) (link) (id) (title) (abstract) (publishdate) (updatedate) (author) (names) (doi) (comment) (journal) (categories))
      (condition-case exception
	  (progn
	    (setq arxiv-query-total-results (string-to-number (arxiv-getxml-context root 'opensearch:totalResults)))
	    (setq arxiv-query-results-min (+ 1 (string-to-number (arxiv-getxml-context root 'opensearch:startIndex))))
	    (setq arxiv-query-results-max (+ arxiv-query-results-min -1 (string-to-number (arxiv-getxml-context root 'opensearch:itemsPerPage))))
	    (when (< arxiv-query-total-results arxiv-query-results-max) (setq arxiv-query-results-max arxiv-query-total-results))
	    (dolist (paper entries)
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
	      (setq author (xml-get-children paper 'author))
	      (setq names (mapcar (lambda (author) (arxiv-getxml-context author 'name)) author))
	      (setq doi (arxiv-getxml-context paper 'arxiv:doi))
	      (setq comment (arxiv-getxml-context paper 'arxiv:comment))
	      (setq journal (arxiv-getxml-context paper 'arxiv:journal_ref))
	      (setq categories (xml-get-children paper 'category))
	      (setq categories (mapcar (lambda (cat) (cdr (assq 'term (xml-node-attributes cat)))) categories))
	      (setq alist-entry `((title . ,title)
				  (author . ,names)
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
	      (setq formated-list (append formated-list (list alist-entry))))
	    formated-list)
	('error (progn
		  (switch-to-buffer (get-buffer-create "*arxiv-debug*"))
		  (print root (get-buffer "*arxiv-debug*"))
		  (error "Cannot parse the API query result: %s. Refer to the debug buffer for information" exception)))))))

(defun arxiv-query (cat date-start date-end &optional start ascending)
  "Query for articles in category CAT between DATE-START and DATE-END.
START specifies starting index (default 0).
If ASCENDING is t then sort the list by ascending order instead of descending."
  (unless (> (string-to-number date-end) (string-to-number date-start))
    (user-error "Incorrect date specification"))
  (arxiv-parse-api (arxiv-geturl-date date-start date-end cat start ascending)))

(defun arxiv-query-author (author &optional cat start)
  "Query arXiv for articles by authors AUTHOR (in category CAT).
START specifies starting index (default 0)."
  (arxiv-parse-api (arxiv-geturl-author author cat start)))

(defun arxiv-query-general (&optional start)
  "Do a general query according to the list `arxiv-query-data-list'.
START specifies starting index (default 0)."
  (arxiv-parse-api (arxiv-get-api-url start)))

(provide 'arxiv-query)
;;; arxiv-query.el ends here

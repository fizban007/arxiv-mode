;; Defining the arxiv-abstract-mode
(require 'arxiv-vars)

(setq arxiv-abstract-mode-map (make-sparse-keymap))
;;======================convinent keymap for iserlohn================================
(define-key arxiv-abstract-mode-map "RET" 'arxiv-open-current-url)
(define-key arxiv-abstract-mode-map "q" '(lambda () 
                                           (interactive)
                                           (delete-window)
                                           (kill-buffer "*arXiv-abstract*")))

(setq arxiv-keyword-list-abstract
      '(("Title:\\(.*?\\)$" . (1 arxiv-title-face))
;;        ("Abstract:\\([[:ascii:]]+?\\)\n\n" . (1 arxiv-abstract-face))
	("\\$[^$]+\\$" 0 arxiv-abstract-math-face t)
        ("Title:\\|Authors:\\|Date:\\|Submitted:\\|Updated:\\|Abstract:\\|Subjects:\\|Journal:\\|DOI:\\|Comments:" . arxiv-keyword-face)))

(defun test-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

;; (defun arxiv-abstract-mode ()
;;   "Major mode for reading arXiv updates online."
;;   (interactive)
;;   (kill-all-local-variables)
;;   (setq major-mode 'arxiv-abstract-mode)
;;   (setq mode-name "Abstract")
;;   ;; (make-local-variable 'paragraph-separate)
;;   ;; (make-local-variable 'paragraph-start)
;;   ;; (make-local-variable 'page-delimiter)
;;   ;; (setq paragraph-start "^Title:")
;;   ;; (setq paragraph-separate " [ \t\^L]*$")
;;   ;; (setq page-delimiter "^Title: ")
;;   (setq font-lock-multiline t)
;;   (setq font-lock-defaults '(arxiv-keyword-list-abstract))
;;   (set-syntax-table arxiv-abstract-syntax-table)
;;   ;; (use-local-map arxiv-abstract-mode-map)
;;   (run-hooks 'arxiv-abstract-mode-hook))
(define-derived-mode arxiv-abstract-mode text-mode "arXiv-abstract"
  "Major mode for reading arXiv abstracts."
  (set (make-local-variable 'font-lock-defaults) '(arxiv-keyword-list-abstract))
  (setq font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions 'test-font-lock-extend-region)
)
  

(provide 'arxiv-abstract)

;;; arxiv-abstract.el ends here

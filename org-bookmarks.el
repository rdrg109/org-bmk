;; TODO
;;
;; lint:
;;   + every search engine must have at least one space character.

(require 'cl-lib)

(defvar org-bmk-frontend 'tridactyl
  "Variable which defines the formatting for the content of the
  files `org-bmk-file-bookmarks' and
  `org-bmk-file-search-engines'.")

(defvar org-bmk-files '("~/my/org/Bookmark manager/Bookmarks.org")
  "Org files which stores the bookmarks.")

(defvar org-bmk-file-bookmarks "~/.config/tridactyl/tridactyl-bookmarks.vim"
  "File in which bookmarks are to be saved.

Don't modify this file manually since its content is deleted
after executing `org-bmk-export-bookmarks'.")

(defvar org-bmk-file-search-engines "~/.config/tridactyl/tridactyl-search-engines.vim"
  "File in which search engines are to be saved.

Don't modify this file manually since its content is deleted
3after executing `org-bmk-export-search-engines'.")

(defvar org-bmk-property-alias "alias"
  "Headline property which contain the alias for the current heading")

(defvar org-bmk-property-alias-prefix "alias-prefix"
  "Headline property which contain the alias that children
  inherit.")

(defvar org-bmk-property-bookmarks "url"
  "Headline property that stores the URL.

This property is required for a heading to be considered a bookmark.")

(defvar org-bmk-property-search-engines "search"
  "Headline property that stores the URL.

The space character is used as the location for the placeholders.

This property is required for a heading to be considered a bookmark.")

(defun org-bmk-export-bookmarks ()
  "Export all bookmarks to `org-bmk-file-bookmarks'."
  (interactive)
  (let ((bookmarks))
    (dolist (file org-bmk-files)
      (with-current-buffer (find-file-noselect file)
	(org-map-entries (lambda ()
			   (interactive)
			   (let (shortcut url alias-prefix alias)
			     (setq url (org-entry-get nil org-bmk-property-bookmarks))
			     (when url
			       (setq alias-prefix (org-entry-get nil org-bmk-property-alias-prefix t))
			       (setq alias (org-entry-get nil org-bmk-property-alias))
			       (when (or alias-prefix alias)
				 (setq shortcut (org-bmk-build-shortcut alias-prefix alias))
				 (unless (assoc shortcut bookmarks)
				   (add-to-list 'bookmarks `(,shortcut . ,url)))))))))
      (with-current-buffer (find-file-noselect org-bmk-file-bookmarks)
	(erase-buffer)
	(dolist (bookmark bookmarks)
	  (let ((shortcut (car bookmark))
		(url (cdr bookmark)))
	    (insert (format "set searchurls.%s %s\n" shortcut url))))
	(save-buffer)))))

(defun org-bmk-export-search-engines ()
  "Export all search engines to `org-bmk-file-search-engines'."
  (interactive)
  (let ((search-engines))
    (dolist (file org-bmk-files)
      (with-current-buffer (find-file-noselect file)
	(org-map-entries (lambda ()
			   (interactive)
			   (let (shortcut search-engine alias-prefix alias)
			     (setq search-engine (org-entry-get nil org-bmk-property-search-engines))
			     (when search-engine
			       (setq alias-prefix (org-entry-get nil org-bmk-property-alias-prefix t))
			       (setq alias (org-entry-get nil org-bmk-property-alias))
			       (when (or alias-prefix alias)
				 (setq shortcut (org-bmk-build-shortcut alias-prefix alias))
				 (unless (assoc shortcut search-engines)
				   (when (and (string-match "^\"" search-engine)
					      (string-match "\"\\'" search-engine))
				     (setq search-engine (replace-regexp-in-string "\"\\'" "" (replace-regexp-in-string "^\"" "" search-engine))))
				   (let ((nspaces (cl-count ?  search-engine)) count)
				     (if (eq nspaces 1)
					 (setq search-engine (replace-regexp-in-string " " "%s" search-engine))
				       (if (> nspaces 1)
					   (progn
					     (setq count 0)
					     (setq search-engine (replace-regexp-in-string " " (lambda (_) (format "%%s%d" (cl-incf count))) search-engine)))))
				   (add-to-list 'search-engines `(,shortcut . ,search-engine)))))))))
      (with-current-buffer (find-file-noselect org-bmk-file-search-engines)
	(erase-buffer)
	(dolist (search-engine search-engines)
	  (let ((shortcut (car search-engine))
		(url (cdr search-engine)))
	    (insert (format "set searchurls.%s %s\n" shortcut url))))
	(save-buffer))))))

(defun org-bmk-build-shortcut (alias-prefix alias)
  "Build the shortcut given a alias and the alias prefix.
Returns nil when the two arguments are nil."
  (let (shortcut)

    ;; Someties some headings set alias-prefix to an empty value to
    ;; overwrite the inherited value. Doing this causes the
    ;; alias-prefix property to equal the empty string.

    (when (equal alias-prefix "")
      (setq alias-prefix nil))

    (when alias-prefix
      (setq alias-prefix (replace-regexp-in-string " " "-" alias-prefix)))
    
    (when (or alias alias-prefix)
      (catch 'shortcut-set
	(when (and alias alias-prefix)
	  (setq shortcut (concat alias-prefix "-" alias))
	  (throw 'shortcut-set t))
	(when alias-prefix
	  (setq shortcut alias-prefix))
	(when alias
	  (setq shortcut alias))))
    shortcut))

(defun org-bmk-build-shortcut-subtree-at-point ()
  "Build the shortcut for the subtree at point."
  (interactive)
  (let (alias-prefix alias shortcut)
    (if (or (org-entry-get nil org-bmk-property-bookmarks)
	    (org-entry-get nil org-bmk-property-search-engines))
	(progn
	  (setq alias (org-entry-get nil org-bmk-property-alias))
	  (setq alias-prefix (org-entry-get nil org-bmk-property-alias-prefix t))
	  (setq shortcut (org-bmk-build-shortcut alias-prefix alias))
	  (message "Shortcut: %s" shortcut))
      (message "The subtree at point doesn't have the required
      properties to be considered a search engine or a
      bookmark."))))

(provide 'org-bookmarks)

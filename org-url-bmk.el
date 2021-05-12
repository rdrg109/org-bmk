(require 'cl-lib)
(require 'ivy)

(defvar org-url-bmk-frontend 'tridactyl
  "Value which defines the formatting for the correct
  definitions of bookmarks and search engines in the files
  `org-url-bmk-file-bookmarks' and
  `org-url-bmk-file-search-engines', respectively.")

(defvar org-url-bmk-files nil
  "Org files which stores the bookmarks.

These files are then used to perform the following actions:

+ Export and index bookmarks.
+ Export search engines.")

(defvar org-url-bmk-file-bookmarks nil
  "File in which bookmarks are to be saved.

Don't modify this file manually since its content is deleted
after executing `org-url-bmk-export-bookmarks'.")

(defvar org-url-bmk-file-search-engines nil
  "File in which search engines are to be saved.

Don't modify this file manually since its content is deleted
after executing `org-url-bmk-export-search-engines'.")

(defvar org-url-bmk-property-alias "alias"
  "Headline property which contain the alias for the current
  heading.")

(defvar org-url-bmk-property-alias-prefix "alias-prefix"
  "Headline property which contain the alias that children
  inherit.")

(defvar org-url-bmk-property-bookmarks "url"
  "Headline property that stores the URL.

Having this property is the only requiremenet for a heading to be
considered a bookmark.")

(defvar org-url-bmk-property-search-engines "search"
  "Headline property that stores the URL.

An space character or matching curly braces can be used as
locations for placeholders.

Having this property is the only requirement for a heading to be
considered a search engine.")

(defvar org-url-bmk-prefix-bookmarks nil
  "String that prefix the shortcut of each bookmark.")

(defvar org-url-bmk-prefix-search-engines nil
  "String that prefix the shortcut of each search engine.")

(defvar org-url-bmk-cache-data-properties t
  "Defines whether URLs need to be saved along with each bookmark
or search engine in `org-url-bmk-indexed-bookmarks' and
`org-url-bmk-indexed-search-engines'.

If set to nil, then a marker is stored for each bookmark or
search engine. Doing this implies that the file that stores your
bookmarks need to remain open in order to be able to retrieve the
corresponding URL when selecting a bookmark or search engine.")

(defvar org-url-bmk-indexed-bookmarks nil
  "Stores indexed bookmarks.")

(defvar org-url-bmk-indexed-search-engines nil
  "Stores indexed search engines.")
  
(defun org-url-bmk-sanitize-search-engine (search-engine)
  "Removes the double quotes that are located at the beginning
and end of the search engine.

Recall that some search engines contain these characters to
ensure that the placeholders that might be at the end of the
URL are not lost when executing `org-entry-get'."
  (if (and (string-match "^\"" search-engine)
	   (string-match "\"\\'" search-engine))
      (replace-regexp-in-string "\"\\'" ""
				(replace-regexp-in-string "^\"" ""
							  search-engine))
    search-engine))

(defun org-url-bmk-export-bookmarks ()
  "Export all bookmarks to `org-url-bmk-file-bookmarks'."
  (interactive)
  (let ((bookmarks))
    (dolist (file org-url-bmk-files)
      (with-current-buffer (find-file-noselect file)
	(org-map-entries (lambda ()
			   (interactive)
			   (let (shortcut url alias-prefix alias)
			     (setq url (org-entry-get nil org-url-bmk-property-bookmarks))
			     (when url
			       (setq alias-prefix (org-entry-get nil org-url-bmk-property-alias-prefix t))
			       (setq alias (org-entry-get nil org-url-bmk-property-alias))
			       (when (or alias-prefix alias)
				 (setq shortcut (org-url-bmk-build-shortcut alias-prefix alias))
				 (unless (assoc shortcut bookmarks)
				   (add-to-list 'bookmarks `(,shortcut . ,url)))))))))
      (with-current-buffer (find-file-noselect org-url-bmk-file-bookmarks)
	(erase-buffer)
	(dolist (bookmark bookmarks)
	  (let ((shortcut (car bookmark))
		(url (cdr bookmark)))
	    (insert (format "set searchurls.%s %s\n" (concat org-url-bmk-prefix-bookmarks shortcut) url))))
	(save-buffer)))))

(defun org-url-bmk-export-search-engines ()
  "Export all search engines to `org-url-bmk-file-search-engines'."
  (interactive)
  (let ((search-engines))
    (dolist (file org-url-bmk-files)
      (with-current-buffer (find-file-noselect file)
	(org-map-entries (lambda ()
			   (interactive)
			   (let (search-engine alias-prefix alias)
			     (setq search-engine (org-entry-get nil org-url-bmk-property-search-engines))
			     (when search-engine
			       (setq alias-prefix (org-entry-get nil org-url-bmk-property-alias-prefix t))
			       (setq alias (org-entry-get nil org-url-bmk-property-alias))
			       ;; If one of the following properties have been defined
			       (when (or alias-prefix alias)
				 (let ((shortcut (org-url-bmk-build-shortcut alias-prefix alias)))
				   ;; If the shortcut hasn't been used yet.
				   (unless (assoc shortcut search-engines)
				     (let ((hierarchy (mapconcat 'identity (org-get-outline-path t) " / ")))
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
					 (add-to-list 'search-engines `(,hierarchy ,shortcut ,search-engine)))))))))))))
    (with-current-buffer (find-file-noselect org-url-bmk-file-search-engines)
      (erase-buffer)
      (dolist (search-engine search-engines)
	(let ((hierarchy (car search-engine))
	      (shortcut (nth 1 search-engine))
	      (url (nth 2 search-engine)))
	  (insert (format "\n\" %s\nset searchurls.%s %s\n"
			  hierarchy
			  (concat org-url-bmk-prefix-search-engines shortcut)
			  url))))
      (save-buffer))))

(defun org-url-bmk-build-shortcut (alias-prefix alias)
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

(defun org-url-bmk-build-shortcut-subtree-at-point ()
  "Build the shortcut for the subtree at point."
  (interactive)
  (let (alias-prefix alias shortcut)
    (if (or (org-entry-get nil org-url-bmk-property-bookmarks)
	    (org-entry-get nil org-url-bmk-property-search-engines))
	(progn
	  (setq alias (org-entry-get nil org-url-bmk-property-alias))
	  (setq alias-prefix (org-entry-get nil org-url-bmk-property-alias-prefix t))
	  (setq shortcut (org-url-bmk-build-shortcut alias-prefix alias))
	  (message "Shortcut: %s" shortcut))
      (user-error "The subtree at point doesn't have the required properties to be considered a search engine or a bookmark."))))

(defun org-url-bmk-index-bookmarks ()
  "Index all bookmarks in `org-url-bmk-files'.

All bookmarks metadata is stored in
`org-url-bmk-indexed-bookmarks'.

For a definition on bookmarks, read this."
  (interactive)
  (setq org-url-bmk-indexed-bookmarks nil)
  (dolist (file org-url-bmk-files)
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
	 (let ((property (org-entry-get nil org-url-bmk-property-bookmarks)))
	   (when property
	     (let ((choice (mapconcat 'identity (org-get-outline-path t) " / "))
		   (metadata (if org-url-bmk-cache-data-properties
				 property
			       (point-marker))))
	       (add-to-list 'org-url-bmk-indexed-bookmarks `(,choice . ,metadata))))))
       nil
       'file)
      (if org-url-bmk-cache-data-properties
	  (kill-buffer))))
  (message "bookmarks have been indexed."))

(defun org-url-bmk-index-search-engines ()
  "index all search engines in `org-url-bmk-files'.

all search engines metadata is stored in
`org-url-bmk-indexed-search-engines'.

for a definition on search engines, read this."
  (interactive)
  (setq org-url-bmk-indexed-search-engines nil)
  (dolist (file org-url-bmk-files)
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
	 (let ((property (org-entry-get nil org-url-bmk-property-search-engines)))
	   (when property
	     (let ((choice (mapconcat 'identity (org-get-outline-path t) " / "))
		   (metadata (if org-url-bmk-cache-data-properties
				 property
			       (point-marker))))
	       (add-to-list 'org-url-bmk-indexed-search-engines `(,choice . ,metadata))))))
       nil
       'file)
      (if org-url-bmk-cache-data-properties
	  (kill-buffer))))
  (message "Search engines have been indexed."))

(defun org-url-bmk-prompt-bookmark (action)
  "Prompts the user for a bookmark and perform ACTION."
  (unless (functionp action)
    (user-error "The provided ACTION \"%s\" is not a function." action))
  (ivy-read "Bookmark: " org-url-bmk-indexed-bookmarks
	    :action (lambda (a)
		      (let ((metadata (cdr a))
			    url)
			(if org-url-bmk-cache-data-properties
			    (setq url metadata)
			  (let ((buffer (marker-buffer metadata))
				(setq url (with-current-buffer buffer
					    (goto-char marker)
					    (org-entry-get nil org-url-bmk-property-bookmarks))))))
			(funcall action url)))))

(defun org-url-bmk-prompt-search-engine (action)
  "Prompts the user for a search engine, fill placeholders and
perform ACTION."
  (unless (functionp action)
    (user-error "The provided ACTION \"%s\" is not a function." action))
  (ivy-read "Search engine: " org-url-bmk-indexed-search-engines
	    :action (lambda (a)
		      (let* ((metadata (cdr a))
			     url)
			(if org-url-bmk-cache-data-properties
			    (setq url metadata)
			  (setq url
				(let* ((marker metadata)
				       (buffer (marker-buffer marker)))
				  (with-current-buffer buffer
				    (goto-char marker)
				    (org-entry-get nil org-url-bmk-property-search-engines)))))
			(setq url
			      (org-url-bmk-prompt-placeholders
			       (org-url-bmk-sanitize-search-engine url)))
			(funcall action url)))))

(defun org-url-bmk-prompt-placeholders (url)
  "Given a search engine, each placeholder is replaced by the
value returned of prompting the user."
  (let (occurrence
	start
	end
	query
	placeholder-name)

    (unless (stringp url)
      (error "The provided argument is not a string."))
    
    (with-temp-buffer
      (insert url)
      (goto-char (point-min))
      (while (setq ocurrence (search-forward-regexp "[ {]" nil t))
	(goto-char ocurrence)
	(backward-char)
	
	(setq start (point))
	
	(if (equal (format "%c" (char-after)) "{")
	    (progn
	      (setq end (save-excursion
			  (forward-sexp)
			  (point)))
	      (setq placeholder-name
		    (string-trim
		     (buffer-substring-no-properties (+ start 1) (- end 1)))))
	  (progn
	    (setq end (+ start 1))
	    (setq placeholder-name nil)))

	(replace-regexp "." "" nil start end)

	(if placeholder-name
	    (setq placeholder-value (read-string (concat
						  "Placeholder ("
						  placeholder-name
						  "): ")))
	  (setq placeholder-value (read-string "Placeholder (empty name): ")))
	
	(insert placeholder-value))
      (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'org-url-bmk)

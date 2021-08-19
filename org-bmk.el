(require 'org)

;;; Customization

;;;; Miscelanous

(defcustom org-bmk-files nil
  "Org files which stores the bookmarks.")

(defcustom org-bmk-completion-list-format "File: %f\nLink: %l\n%o"
  "Format used for bookmarks in the completion list")

;;;; Property names

(defcustom org-bmk-property-bookmarks "LINK"
  "Name of the property that stores the link of the
bookmark. This property can contain any link supported by Org
Mode, since its value is opened with `org-open-at-point'.

Having this property is the only requirement for a heading to be
considered a bookmark."
  :type 'string)

(defcustom org-bmk-prompt-function 'org-bmk-prompt-bookmark-helm
  "Function used for prompting for a bookmark."
  :type 'function)

;;; Variables

(defvar org-bmk-bookmarks nil
  "Contains the data of all the indexed bookmarks.")

;;; Functions

(defun org-bmk-store-headline ()
  "Store all the data the bookmark at point.

Precondition:
+ The headline has a LINK property"
  (let (alist)
    (push (cons 'file (buffer-file-name)) alist)
    (push (cons 'outline (org-get-outline-path t)) alist)
    (push (cons 'link (org-entry-get nil "LINK")) alist)
    (push (cons 'tags (org-get-tags)) alist)
    (push alist org-bmk-bookmarks)))

(defun org-bmk-index-bookmarks ()
  "Store all the bookmarks in `org-bmk-bookmarks'.

This function indexes the bookmarks so that other functions can
query the bookmarks from this variable without the need of
traversing all the files again."
  (setq org-bmk-bookmarks nil)
  (dolist (file org-bmk-files)
    (message "Traversing %s" file)
    (with-current-buffer (find-file-noselect file)
      (org-map-entries 'org-bmk-store-headline "LINK={.}"))))

(defun org-bmk-open-link (link)
  "Open an Org Mode link."
  (with-temp-buffer
    (org-mode)
    (insert link)
    (beginning-of-buffer)
    (org-open-at-point)))

(defun org-bmk-format-outline (outline)
  "Given a list representing the outline of an Org Mode heading,
format it in a more readable format."
  (let (result)
    (dotimes (i (length outline))
      (let ((prefix (propertize
                     (concat
                      (number-to-string i)
                      ". ")
                     ;; We use line-prefix for the indentation of each
                     ;; level, so that we can use the "^" operator for
                     ;; matching the depth of the headline.
                     'line-prefix
                     (make-string (* i 2) ? )))
            (content (nth i outline)))
        (push (format "%s%s" prefix content) result)))
    (string-join (reverse result) "\n")))

(defun org-bmk-build-candidates-list ()
  "Build the candidates list."
  (let (candidates)
    (dolist (bookmark org-bmk-bookmarks)
      (let ((file (cdr (assq 'file bookmark)))
	    (outline (cdr (assq 'outline bookmark)))
	    (link (cdr (assq 'link bookmark)))
            (tags (cdr (assq 'tags bookmark)))
	    candidate)
        (setq candidate (cons (org-replace-escapes
		               org-bmk-completion-list-format
		               `(("%f" . ,file)
                                 ("%o" . ,(org-bmk-format-outline outline))
                                 ("%l" . ,link)
                                 ("%t" . ,(org-make-tag-string tags))))
                              link))
        (push candidate candidates)))
    candidates))

(defun org-bmk-prompt-bookmark-ivy (candidates)
  (interactive)
  (ivy-read "Bookmark: "
            candidates
            :action (lambda (x)
                      (let ((link (cdr x)))
                        (org-bmk-open-link link)))))

(defun org-bmk-prompt-bookmark-helm (candidates)
  (interactive)
  (helm :buffer "*helm org-bmk*"
        :sources (helm-build-sync-source "Bookmark"
                   :candidates candidates
                   :multiline t
                   :action 'org-bmk-open-link)))

(defun org-bmk-prompt-bookmark ()
  (interactive)
  "Prompts for a given bookmark."
  (funcall org-bmk-prompt-function (org-bmk-build-candidates-list)))

(provide 'org-bmk)

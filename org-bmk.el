(require 'org)

;;; Customization

;;;; Miscelanous

(defcustom org-bmk-files nil
  "Org files which stores the bookmarks.")

(defcustom org-bmk-completion-list-format '(path-file path-outline link tags)
  "Format used for bookmarks in the completion list")

(defcustom org-bmk-labels
  (list (cons 'path-file (propertize "File\n" 'face 'bold))
        (cons 'path-outline (propertize "Outline\n" 'face' bold))
        (cons 'link (propertize "Link: " 'face' bold))
        (cons 'tags (propertize "Tags: " 'face' bold)))
  "Labelsp for the symbols.")

(defcustom org-bmk-formatters
  (list (cons 'path-file 'org-bmk-formatter-path-file)
        (cons 'path-outline 'org-bmk-formatter-path-outline)
        (cons 'tags 'org-make-tag-string))
  "Formatters for the symbols.")

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

;;; Formatters

(defun org-bmk-formatter-path-file (path)
  (let (result)
    (setq path (split-string path "/" t))
    (dotimes (i (length path))
      (let ((line-prefix (make-string (* i 2) ? ))
            (content (nth i path)))
        (push (propertize content 'line-prefix line-prefix) result)))
    (string-join (reverse result) "\n")))

(defun org-bmk-formatter-path-outline (path)
  "Given a list representing the outline of an Org Mode heading,
format it in a more readable format."
  (let (result)
    (dotimes (i (length path))
      (let ((prefix (propertize
                     (concat
                      (number-to-string i)
                      ". ")
                     ;; We use line-prefix for the indentation of each
                     ;; level, so that we can use the "^" operator for
                     ;; matching the depth of the headline.
                     'line-prefix
                     (make-string (* i 2) ? )))
            (content (nth i path)))
        (push (format "%s%s" prefix content) result)))
    (string-join (reverse result) "\n")))

;;; Functions

(defun org-bmk-store-headline ()
  "Store the information of a headline.

This function is intended to be executed in a headline. A
precondition for this function is that the headline contains a
LINK property."
  (let (alist)
    (push (cons 'path-file (buffer-file-name)) alist)
    (push (cons 'path-outline (org-get-outline-path t)) alist)
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

(defun org-bmk-format-bookmark (bookmark)
  "Format the information of a given bookmark according to the
format defined by `org-bmk-completion-list-format'"
  (let (items formatter content label)
    (dolist (item org-bmk-completion-list-format)
      (catch 'next
        (setq content (cdr (assq item bookmark)))
        ;; If the bookmark doesn't have that information, then nothing
        ;; needs to be done.
        (unless content
          (throw 'next t))
        (setq formatter (cdr (assq item org-bmk-formatters)))
        (setq label (cdr (assq item org-bmk-labels)))
        ;; If a formatter exists, then use the formatter.
        (when formatter
          (setq content (funcall formatter content)))
        ;; When a label has been defined, then insert it before the
        ;; content.
        (when label
          (setq content (concat label content)))
        (push content items)))
    ;; We reverse the list because "dolist" starts at the first item
    ;; of the list but "push" sequentially inserts items at the front.
    (string-join (reverse items) "\n")))

(defun org-bmk-build-candidates-list ()
  "Build the candidates list."
  (let (candidate candidates)
    (dolist (bmk org-bmk-bookmarks)
      (setq candidate (cons (org-bmk-format-bookmark bmk)
                            (cdr (assq 'link bmk))))
      (push candidate candidates))
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

;;; Interactive functions

(defun org-bmk-prompt-bookmark ()
  (interactive)
  "Prompts for a given bookmark."
  (funcall org-bmk-prompt-function (org-bmk-build-candidates-list)))

(provide 'org-bmk)

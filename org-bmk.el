(require 'org)

;;; Customization

;;;; Miscelanous

(defcustom org-bmk-files nil
  "Org files which stores the bookmarks.")

(defcustom org-bmk-completion-list-format '(path-file path-outline link tags description)
  "Format used for bookmarks in the completion list")

(defcustom org-bmk-labels
  (list (cons 'path-file (propertize "File\n" 'face 'bold))
        (cons 'path-outline (propertize "Outline\n" 'face' bold))
        (cons 'link (propertize "Link: " 'face' bold))
        (cons 'tags (propertize "Tags: " 'face' bold))
        (cons 'description (propertize "Description: " 'face' bold)))
  "Labels for the symbols.")

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

(defvar org-bmk-prompt-bookmark-candidates nil
  "Stores the candidates so that they are not obtained whenever
  the function is called.")

;;; Functions

(defun org-bmk-index-link-at-point ()
  "Store the information of the link at point."
  (let ((path-file (buffer-file-name))
        (element (org-element-context))
        alist link description tags path-outline)

    ;; If the link is at the top level of the document, then it
    ;; doesn't have neither tags nor an outline.

    ;; FIXME: org-get-tags doesn't get the tags when in the top level
    ;; of the document even when #+FILE_TAGS have set the tags for the
    ;; whole document. Is this expected behavior?

    (when (org-current-level)
      (setq path-outline (org-get-outline-path t))
      (setq tags (org-get-tags)))

    ;;; Get the link and the description
    ;;
    ;; If the link is at a property node, then we need to retrieve the
    ;; link beforehand.
    ;;
    ;; FIXME: Repeated code


    (if (eq (car element) 'node-property)
        (progn
          (setq link (org-element-property :value element))
          (with-temp-buffer
            (org-mode)
            (insert link)
            (backward-char)
            ;; Once we retrieve the link, we obtain "element" again.
            (setq element (org-element-context))
            (setq link (org-element-property :raw-link element))
            (setq description-begin (org-element-property :contents-begin element))
            (setq description-end (org-element-property :contents-end element))
            (setq description (and description-begin
                                   description-end
                                   (buffer-substring-no-properties
                                    description-begin
                                    description-end)))))
      (progn
        (setq link (org-element-property :raw-link element))
        (setq description-begin (org-element-property :contents-begin element))
        (setq description-end (org-element-property :contents-end element))
        (setq description (and description-begin
                               description-end
                               (buffer-substring-no-properties
                                description-begin
                                description-end)))))

    ;;; Save the data

    (push (cons 'path-file path-file) alist)
    (push (cons 'link link) alist)

    ;; A link can be at the top level of the document which causes the
    ;; outline to its position in the outline to be nil.
    (when path-outline
      (push (cons 'path-outline path-outline) alist))
    (when tags
      (push (cons 'tags tags) alist))
    (when description
      (push (cons 'description description) alist))
    (push alist org-bmk-bookmarks)))

(defun org-bmk-index-bookmarks-in-file (file)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward org-any-link-re nil t)
        (catch 'next
          ;; If the link is at a source block¸ then don't consider
          ;; it.
          ;;
          ;; FIXME: Use org-context instead, because, aparently, it is
          ;; faster.
          (when (or (eq (car (org-element-context)) 'example-block)
                    (eq (car (org-element-context)) 'src-block))
            (throw 'next t))
          (backward-char)
          (org-bmk-index-link-at-point))))))

(defun org-bmk-index-bookmarks ()
  "Store all the bookmarks in `org-bmk-bookmarks'.

This function indexes the bookmarks so that other functions can
query the bookmarks from this variable without the need of
traversing all the files again."
  (setq org-bmk-bookmarks nil)
  (dolist (file org-bmk-files)
    (message "Traversing %s" file)
    (org-bmk-index-bookmarks-in-file file)))

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
                        (org-open-link-from-string link)))))

(defun org-bmk-prompt-bookmark-helm (candidates)
  (interactive)
  (helm :buffer "*helm org-bmk*"
        :sources (helm-build-sync-source "Bookmark"
                   :candidates candidates
                   :multiline t
                   :action 'org-open-link-from-string)))


;;;; Formatters

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

;;;; Interactive functions

(defun org-bmk-prompt-bookmark ()
  "Prompts for a given bookmark. The candidates for the prompt
function are computed whenever this function is called. See
`org-bmk-prompt-bookmark-use-cache' if you would rather use cache
instead."
  (interactive)
  (funcall org-bmk-prompt-function (org-bmk-build-candidates-list)))

(defun org-bmk-prompt-bookmark-use-cache (&optional arg)
  "Prompts for a given bookmark by using the cache information.

With `\\[universal-argument], the cache is refreshed."
  (interactive "p")
  (when (or (> arg 1) (null org-bmk-prompt-bookmark-cache))
    (setq org-bmk-prompt-bookmark-cache (org-bmk-build-candidates-list)))
  (funcall org-bmk-prompt-function org-bmk-prompt-bookmark-cache))

(provide 'org-bmk)

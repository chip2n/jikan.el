;; -*- lexical-binding: t -*-

(require 's)
(require 'cl-lib)
(require 'request)
(require 'ivy-rich)

(defvar jikan--current-entries nil)

(cl-defstruct (jikan--entry (:constructor jikan--make-entry))
  title synopsis url)

(defun jikan-search (name)
  (interactive "MTitle: ")
  ;; this-command inside sentinels appears to be nil, so we need to
  ;; rebind it in order for ivy to find the correct display function
  (let ((caller this-command))
    (request
      (format "https://api.jikan.moe/v3/search/anime?q=%s&page=1" (url-hexify-string name))
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown)))
      :success (cl-function (lambda (&key data &allow-other-keys)
                               ;; in order to allow users to quit the ivy prompt
                               ;; with C-g, we need to wrap callback with this
                               (with-local-quit
                                 (jikan--process-search-result data caller)))))))

(defun jikan--process-search-result (data caller)
  (let* ((jikan--current-entries
          (mapcar (lambda (result)
                    (jikan--make-entry
                     :title (assoc-default 'title result)
                     :synopsis (assoc-default 'synopsis result)
                     :url (assoc-default 'url result)))
                  (assoc-default 'results data)))
         (titles (mapcar #'jikan--entry-title jikan--current-entries))
         (ivy-sort-functions-alist nil)
         (this-command caller))
    (completing-read "Select a title: " titles)))

(defun jikan--get-synopsis (title)
  (let ((entry (find-if (lambda (x) (string-equal (jikan--entry-title x) title))
                        jikan--current-entries)))
    (jikan--entry-synopsis entry)))

(defun jikan--display-transformer (title)
  (ivy-rich-format title '((identity (:width 40))
                           (jikan--get-synopsis (:face font-lock-doc-face)))))

(ivy-configure 'jikan-search
  :display-transformer-fn #'jikan--display-transformer)

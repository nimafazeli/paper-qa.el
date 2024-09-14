;;; paper-qa.el --- Emacs interface for paper-qa with Zotero integration -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.4
;; Package-Requires: ((emacs "26.1") (jupyter "0.8.2") (helm "3.8.0") (auth-source "1.0"))
;; Keywords: tools, research
;; URL: https://github.com/yourusername/paper-qa.el

;;; Commentary:

;; This package provides an Emacs interface for querying your Zotero collections using paper-qa.
;; It leverages the Zotero integration provided by paper-qa and simplifies the workflow.

;;; Code:

(require 'jupyter)
(require 'helm)
(require 'auth-source)
(require 'json)

(defgroup paper-qa nil
  "Customization options for paper-qa Emacs interface."
  :group 'applications)

(defcustom paper-qa-python-command "python3"
  "Python executable to use for the Jupyter kernel."
  :type 'string
  :group 'paper-qa)

(defvar paper-qa--kernel nil
  "Jupyter kernel used for paper-qa interactions.")

(defvar paper-qa--zotero-collections nil
  "List of Zotero collections.")

(defvar paper-qa--docs-initialized nil
  "Flag indicating whether the Docs object has been initialized.")

(defvar paper-qa--current-collection nil
  "The currently selected Zotero collection.")

(defun paper-qa--get-zotero-credentials ()
  "Retrieve Zotero credentials from auth-source or prompt the user."
  (let ((auth (nth 0 (auth-source-search :host "zotero" :max 1))))
    (if auth
        (list (plist-get auth :user) (funcall (plist-get auth :secret)))
      (list (read-string "Enter your Zotero User ID: ")
            (read-passwd "Enter your Zotero API Key: ")))))

(defun paper-qa--start-kernel ()
  "Start the Jupyter kernel for paper-qa if it's not running."
  (unless (and paper-qa--kernel (jupyter-live-p paper-qa--kernel))
    (setq paper-qa--kernel
          (jupyter-start-python-repl paper-qa-python-command nil :name "paper-qa-kernel"))
    (message "Starting paper-qa Jupyter kernel...")
    (sleep-for 2) ;; Wait a bit for the kernel to start
    (paper-qa--initialize-paper-qa)))

(defun paper-qa--initialize-paper-qa ()
  "Initialize paper-qa in the Jupyter kernel with Zotero integration."
  (let* ((credentials (paper-qa--get-zotero-credentials))
         (user-id (car credentials))
         (api-key (cadr credentials)))
    (jupyter-eval-string
     (format "
import os
os.environ['ZOTERO_USER_ID'] = '%s'
os.environ['ZOTERO_API_KEY'] = '%s'
from paperqa import Docs
from paperqa.contrib import ZoteroDB

zotero_db = ZoteroDB()
docs = Docs()
collections = zotero_db.collections()
collection_names = [c['data']['name'] for c in collections]
" user-id api-key)
     paper-qa--kernel)
    (setq paper-qa--docs-initialized t)
    (message "paper-qa initialized with Zotero integration.")))

(defun paper-qa--fetch-collections ()
  "Fetch and parse Zotero collections from the Jupyter kernel."
  (unless paper-qa--zotero-collections
    (let ((result (jupyter-eval-sync "collection_names" paper-qa--kernel)))
      (setq paper-qa--zotero-collections
            (condition-case nil
                (json-read-from-string (replace-regexp-in-string "'" "\"" (string-trim result)))
              (error
               (message "Error parsing Zotero collections")
               nil))))))

(defun paper-qa--select-collection ()
  "Use Helm to select a Zotero collection."
  (paper-qa--fetch-collections)
  (helm :sources (helm-build-sync-source "Zotero Collections"
                   :candidates paper-qa--zotero-collections
                   :fuzzy-match t)
        :buffer "*helm Zotero Collections*"))

(defun paper-qa--add-papers-from-collection (collection-name)
  "Add papers from COLLECTION-NAME to the Docs object."
  (setq paper-qa--current-collection collection-name)
  (message "Adding papers from collection '%s'..." collection-name)
  (jupyter-eval-string
   (format "
docs.clear()
for item in zotero_db.iterate(collection_name=\"%s\"):
    if item.pdf:
        docs.add(item.pdf, metadata={'title': item.title})
" collection-name)
   paper-qa--kernel)
  (message "Papers added to Docs object."))

;;;###autoload
(defun paper-qa-ask (query)
  "Ask QUERY using paper-qa on the selected Zotero collection."
  (interactive "sEnter your query: ")

  ;; Automatically start the kernel if it’s not running
  (unless (and paper-qa--kernel (jupyter-live-p paper-qa--kernel))
    (paper-qa--start-kernel))

  ;; Initialize paper-qa if not already done
  (unless paper-qa--docs-initialized
    (paper-qa--initialize-paper-qa))

  ;; Prompt for Zotero collection if not selected
  (unless paper-qa--current-collection
    (let ((collection-name (paper-qa--select-collection)))
      (paper-qa--add-papers-from-collection collection-name)))

  ;; Run the query and display the result
  (let ((result (jupyter-eval-sync
                 (format "
answer = docs.query(\"\"\"%s\"\"\")
print(answer.answer)
" query)
                 paper-qa--kernel)))
    (with-output-to-temp-buffer "*paper-qa Answer*"
      (princ result))))

;;;###autoload
(defun paper-qa-clear-cache ()
  "Clear cached collection and reset Docs object."
  (interactive)
  (setq paper-qa--current-collection nil)
  (setq paper-qa--zotero-collections nil)
  (setq paper-qa--docs-initialized nil)
  (when (and paper-qa--kernel (jupyter-live-p paper-qa--kernel))
    (jupyter-eval-string "docs.clear()" paper-qa--kernel))
  (message "paper-qa cache cleared."))

;;;###autoload
(defun paper-qa-stop-kernel ()
  "Stop the paper-qa Jupyter kernel."
  (interactive)
  (when (and paper-qa--kernel (jupyter-live-p paper-qa--kernel))
    (jupyter-stop-repl paper-qa--kernel)
    (setq paper-qa--kernel nil)
    (setq paper-qa--docs-initialized nil)
    (setq paper-qa--current-collection nil)
    (setq paper-qa--zotero-collections nil)
    (message "paper-qa Jupyter kernel stopped.")))

(provide 'paper-qa)

;;; paper-qa.el ends here

;;; paper-qa.el --- Interface for paper-qa in Emacs -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (jupyter "0.8.2") (helm "3.8.0"))
;; Keywords: tools
;; URL: https://github.com/nimafazeli/paper-qa.el

;;; Commentary:
;; This package provides an interface to paper-qa within Emacs.

;;; Code:

(require 'jupyter)
(require 'helm)
(require 'auth-source)

(defgroup paper-qa nil
  "Customization group for paper-qa."
  :group 'applications)

(defcustom paper-qa-zotero-user-id nil
  "Zotero user ID for API access."
  :type 'string
  :group 'paper-qa)

(defcustom paper-qa-zotero-api-key nil
  "Zotero API key for API access."
  :type 'string
  :group 'paper-qa)

(defcustom paper-qa-python-venv "~/.emacs_venv"
  "Path to Python virtual environment for paper-qa."
  :type 'string
  :group 'paper-qa)

(defvar paper-qa-kernel nil
  "Jupyter kernel for paper-qa interactions.")

(defvar paper-qa-python-command
  (expand-file-name ".emacs_venv/Scripts/python.exe" (getenv "HOME"))
  "Path to Python executable with paper-qa installed.")

(defvar paper-qa-zotero-path
  (expand-file-name "Zotero" (getenv "HOME"))
  "Path to Zotero library.")

(defvar paper-qa-docs nil
  "The current paper-qa Docs object.")

(defvar paper-qa-zotero-collections nil
  "List of Zotero collections.")

(defun paper-qa-get-zotero-credentials ()
  "Retrieve Zotero credentials from custom variables."
  (list paper-qa-zotero-user-id paper-qa-zotero-api-key))

(defun paper-qa-init ()
  "Initialize paper-qa kernel and Zotero integration."
  (interactive)
  (let* ((credentials (paper-qa-get-zotero-credentials))
         (user-id (car credentials))
         (api-key (cadr credentials))
         (jupyter-command (expand-file-name "Scripts/jupyter" paper-qa-python-venv)))
    (unless (file-exists-p jupyter-command)
      (error "Jupyter executable not found. Please ensure it's installed in %s" paper-qa-python-venv))
    (setq paper-qa-kernel (jupyter-run-repl jupyter-command nil nil))
    (jupyter-repl-associate-buffer paper-qa-kernel)
    (jupyter-send-code-cell paper-qa-kernel
     (format "import os
import paperqa
from paperqa import Docs
from paperqa.contrib import ZoteroDB
os.environ['ZOTERO_USER_ID'] = '%s'
os.environ['ZOTERO_API_KEY'] = '%s'
zotero_db = ZoteroDB()
docs = Docs()

def get_collections():
    collections = zotero_db.collections()
    return [c['data']['name'] for c in collections]

collections = get_collections()" user-id api-key))
    (setq paper-qa-docs t)
    (setq paper-qa-zotero-collections
          (jupyter-eval "collections" :cell-type :code))))

(defun paper-qa-query (query)
  "Query paper-qa with QUERY."
  (interactive "sEnter your query: ")
  (unless paper-qa-docs
    (error "Paper-qa not initialized. Run M-x paper-qa-init first"))
  (jupyter-send-code-cell paper-qa-kernel
   (format "result = docs.query('%s')
print(result.answer)" query))
  (jupyter-repl-pop-to-buffer))

(defun paper-qa-fetch-by-collection ()
  "Fetch papers from a Zotero collection."
  (interactive)
  (unless paper-qa-zotero-collections
    (error "Zotero collections not fetched. Run M-x paper-qa-init first"))
  (let ((collection (completing-read "Select collection: " paper-qa-zotero-collections)))
    (jupyter-send-code-cell paper-qa-kernel
     (format "for item in zotero_db.iterate(collection_name='%s', limit=20):
    docs.add(item.pdf, docname=item.key)
print('Added papers from collection \"%s\" to the Docs object.')" collection collection))))

(provide 'paper-qa)

;;; paper-qa.el ends here
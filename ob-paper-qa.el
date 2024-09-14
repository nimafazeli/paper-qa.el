;;; ob-paper-qa.el --- Org-babel functions for paper-qa evaluation

;; Copyright (C) 2024 Your Name

;; Author: Your Name <your.email@example.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/yourusername/ob-paper-qa
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-babel support for evaluating paper-qa queries.
;; Requires the paper-qa CLI tool to be installed and available in the system PATH.

;;; Code:
(require 'ob)
(require 'json)

(defvar org-babel-default-header-args:paper-qa
  '((:results . "raw")
    (:exports . "both")))

(defun org-babel-execute:paper-qa (body params)
  "Execute a block of Paper-QA code with org-babel.
BODY contains the query text.
PARAMS are the header arguments."
  (let* ((docs-dir (or (cdr (assq :docs-dir params)) (expand-file-name "~/papers")))
         (index (or (cdr (assq :index params)) "default"))
         (zotero-collection (cdr (assq :zotero-collection params)))
         (temperature (or (cdr (assq :temperature params)) "0.0"))
         (answer-length (or (cdr (assq :answer-length params)) "about 200 words"))
         (evidence-k (or (cdr (assq :evidence-k params)) "10"))
         (query body)
         (python-cmd
          (format
           "
import os
from pyzotero import zotero
from paperqa import Docs
import paperqa

# Zotero setup
library_id = os.getenv('ZOTERO_USER_ID')
api_key = os.getenv('ZOTERO_API_KEY')
zot = zotero.Zotero(library_id, 'user', api_key)

# PaperQA setup
docs = Docs()

# Fetch papers from Zotero collection
if '%s':
    items = zot.collection_items('%s', format='json')
    for item in items:
        if 'contentType' in item['data'] and item['data']['contentType'] == 'application/pdf':
            attachment = zot.item(item['key'], format='json')
            if attachment:
                file_path = attachment['data']['filename']
                docs.add(file_path)

# Execute query
answer = docs.query('%s', k=%s)
print(answer.answer)
"
           zotero-collection
           zotero-collection
           query
           evidence-k)))
    (with-temp-buffer
      (call-process-region python-cmd nil "python" nil t nil "-c" python-cmd)
      (buffer-string))))

(defun paper-qa-list-zotero-collections ()
  "List available Zotero collections using pyzotero."
  (interactive)
  (with-temp-buffer
    (call-process "python" nil t nil
                  "-c"
                  "
import os
from pyzotero import zotero

library_id = os.getenv('ZOTERO_USER_ID')
api_key = os.getenv('ZOTERO_API_KEY')
zot = zotero.Zotero(library_id, 'user', api_key)

collections = zot.collections()
for collection in collections:
    print(collection['data']['name'])
")
    (buffer-string)))

(defun paper-qa-select-zotero-collection ()
  "Select a Zotero collection interactively."
  (interactive)
  (let ((collections (split-string (paper-qa-list-zotero-collections) "\n" t)))
    (completing-read "Select Zotero collection: " collections)))

(provide 'ob-paper-qa)
;;; ob-paper-qa.el ends here

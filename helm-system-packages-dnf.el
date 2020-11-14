;;; helm-system-packages-dnf.el --- Helm UI for RPM-based distros using DNF. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Damien Cassou <damien@cassou.me>

;; Author: Damien Cassou <damien@cassou.me>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.10.2
;; Package-Requires: ((emacs "24.4") (helm "2.8.6"))

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
;; Helm UI for RPM-based distributions using DNF.

;;; Code:
(require 'helm-system-packages)

(require 'seq)

(defcustom helm-system-packages-dnf-actions
  (list
   (cons "Show package(s)" #'helm-system-packages-dnf-info)
   (cons "Install"  #'helm-system-packages-dnf-install)
   (cons "Uninstall" #'helm-system-packages-dnf-uninstall)
   (cons "Browse homepage URL" #'helm-system-packages-dnf-browse-url)
   (cons "Find files" #'helm-system-packages-dnf-find-files))
  "Actions for Helm DNF."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-dnf--package-attributes-to-org-description-list ()
  "Convert \"Key   : value\" to \"- Key :: value\".
This is org format for description list items."
  (goto-char (point-min))
  (while (re-search-forward "^\\(\\<[^ ]*\\) +: \\(.*\\)$" nil t)
    (let ((key (match-string-no-properties 1))
          (value (match-string-no-properties 2)))
      (replace-match (format "- %s :: %s" key value) nil t))))

(defun helm-system-packages-dnf--info (packages)
  "Return a list of (NAME . DESC) describing PACKAGES."
  (save-match-data
    (with-temp-buffer
      (apply #'process-file "dnf" nil t nil "info" packages)
      (goto-char (point-min))
      (helm-system-packages-dnf--merge-descriptions)
      (helm-system-packages-dnf--package-attributes-to-org-description-list)
      (goto-char (point-min))
      (cl-loop
       while (re-search-forward "^- Name :: \\(.*\\)$" nil t)
       collect (cons (match-string-no-properties 1)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (save-match-data
                        (re-search-forward "^$")
                        (match-beginning 0))))))))

(defun helm-system-packages-dnf-info (candidate)
  "Print information about helm CANDIDATE.
With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (helm-system-packages-show-information
   `((uninstalled . ,(helm-system-packages-dnf--info
                      (if helm-in-persistent-action
                          (list candidate)
                        (helm-marked-candidates)))))))

(defun helm-system-packages-dnf-install (_)
  "Install marked candidates."
  (helm-system-packages-run-as-root "dnf" "install"))

(defun helm-system-packages-dnf-uninstall (_)
  "Uninstall marked candidates."
  (helm-system-packages-run-as-root "dnf" "remove"))

(defun helm-system-packages-dnf-browse-url (_)
  "Print homepage URLs of `helm-marked-candidates'.
With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (helm-system-packages-browse-url
   (save-match-data
     (with-temp-buffer
       (apply #'process-file "dnf" nil t nil "info" (helm-marked-candidates))
       (goto-char (point-min))
       (cl-loop
        while (re-search-forward "^URL *: \\(.*\\)$" nil t)
        collect (match-string-no-properties 1) into urls
        finally return (seq-uniq urls #'string=))))))

(defun helm-system-packages-dnf--list-files (package)
  "Return a list of all files installed by PACKAGE."
  (message "Collecting files of %s" package)
  (save-match-data
    (with-temp-buffer
      (process-file "dnf" nil t nil "repoquery" "-l" package)
      (goto-char (point-min))
      (cl-loop
       while (re-search-forward "^\\(/.*\\)$" nil t)
       collect (match-string-no-properties 1)))))

(defun helm-system-packages-dnf-find-files (_)
  "Find files for marked candidates."
  (let* ((package-files (make-hash-table :test #'equal)))
    (dolist (package (helm-marked-candidates))
      (puthash package (helm-system-packages-dnf--list-files package) package-files))
    (helm-system-packages-find-files package-files)))

(defun helm-system-packages-dnf--delete-non-package-lines ()
  "Remove every line of current package that is not a package."
  ;; delete summary line:
  (goto-char (point-min))
  (delete-region (point-min) (line-beginning-position 2)))

(defun helm-system-packages-dnf--merge-descriptions ()
  "Merge description of packages spanning several lines in current buffer."
  (goto-char (point-min))
  (while (re-search-forward "^ *\\(...\\|\\):" nil t)
    (delete-region (line-end-position 0) (point))))

(defun helm-system-packages-dnf--format-packages ()
  "Format each package and description in current buffer.
In particular, descriptions are vertically aligned."
  (goto-char (point-min))
  (save-match-data
    (while (re-search-forward "^\\([^ ]*\\) : \\(.*\\)$" nil t)
      (let ((package-name (match-string 1))
            (package-description (match-string 2)))
        (replace-match
         (format (concat "%-" (number-to-string helm-system-packages-column-width) "s %s")
                 package-name package-description)
         nil t)))))

(defun helm-system-packages-dnf--sort-by-package-name ()
  "Sort packages in current buffer by package name."
  (sort-lines nil (point-min) (point-max)))

(defun helm-system-packages-dnf--list-packages ()
  "List all packages in current buffer with descriptions."
  (process-file "dnf" nil '(t nil) nil "search" "--quiet" "*")
  (helm-system-packages-dnf--delete-non-package-lines)
  (helm-system-packages-dnf--merge-descriptions)
  (helm-system-packages-dnf--format-packages)
  (helm-system-packages-dnf--sort-by-package-name))

(defun helm-system-packages-dnf--remove-descriptions ()
  "Remove all package descriptions in current buffer.
Only package names remain."
  (goto-char (point-min))
  (while (re-search-forward "^[^ ]+" nil t)
    (delete-region (match-end 0) (line-end-position))))

(defun helm-system-packages-dnf-refresh ()
  "Refresh the package list."
  (interactive)
  (save-match-data
    (with-temp-buffer
      (helm-system-packages-dnf--list-packages)
      (let ((descriptions (buffer-string)))
        (helm-system-packages-dnf--remove-descriptions)
        (helm-system-packages--cache-set (buffer-string) descriptions nil "dnf")))))

(defvar helm-system-packages-dnf
  (helm-system-packages-manager-create
   :name "dnf"
   :refresh-function #'helm-system-packages-dnf-refresh
   :dependencies '("dnf")
   :actions helm-system-packages-dnf-actions))

(provide 'helm-system-packages-dnf)

;;; helm-system-packages-dnf.el ends here

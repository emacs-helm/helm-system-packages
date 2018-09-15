;;; helm-system-packages-dnf.el --- Helm UI for RPM-based distros using dnf. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Damien Cassou <damien@cassou.me>

;; Author: Damien Cassou <damien@cassou.me>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.10.1
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
;; Helm UI for RPM-based distributions using dnf.

;;; Code:
(require 'helm-system-packages)

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
   :dependencies '("dnf")))

(provide 'helm-system-packages-dnf)

;;; helm-system-packages-dnf.el ends here

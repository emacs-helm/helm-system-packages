;;; helm-system-packages.el --- Helm UI wrapper for system package managers. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017        Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.6.9
;; Package-Requires: ((emacs "24.4") (helm "2.8.6"))
;; Keywords: helm, packages

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
;; Helm UI wrapper for system package managers.

;;; Code:

;;; Internals

(defvar helm-system-packages-eshell-buffer "*helm-system-packages-eshell*")
(defvar helm-system-packages-buffer "*helm-system-packages-output*")

(defvar helm-system-packages-list-explicit nil)
(defvar helm-system-packages-list-dependencies nil)
(defvar helm-system-packages-list-all nil)

(defvar helm-system-packages--explicit nil)
(defvar helm-system-packages--dependencies nil)
(defvar helm-system-packages--all nil)

(defface helm-system-packages-explicit '((t (:foreground "orange")))
  "Face for excplitly installed packages."
  :group 'traverse-faces)

(defface helm-system-packages-dependencies '((t (:foreground "lightblue")))
  "Face for packages installed as dependencies."
  :group 'traverse-faces)

(defun helm-system-packages-highlight (packages)
  "Highlight all explicitly installed packages as well as dependencies."
  ;; TODO: Transform in-place?
  (mapcar (lambda (pkg)
            (propertize pkg 'face
                        (cond
                         ((member pkg helm-system-packages--explicit) 'helm-system-packages-explicit)
                         ((member pkg helm-system-packages--dependencies) 'helm-system-packages-dependencies)
                         (t nil))))
          packages))

(defun helm-system-packages-run (command &rest args)
  "COMMAND to run over `helm-marked-candidates'."
  (let ((arg-list (append args (helm-marked-candidates))))
    (with-temp-buffer
      (apply #'call-process command nil t nil arg-list)
      (buffer-string))))

(defun helm-system-packages-print (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (let ((res (apply #'helm-system-packages-run command args)))
    (if (string-empty-p res)
        (message "No result")
      (unless helm-current-prefix-arg
        (switch-to-buffer helm-system-packages-buffer)
        (erase-buffer)
        (org-mode)
        (setq res (replace-regexp-in-string "\\`.*: " "* " res))
        (setq res (replace-regexp-in-string "\n\n.*: " "\n* " res)))
      (insert res))))

(defun helm-system-packages-run-as-root (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

COMMAND will be run in an Eshell buffer `helm-system-packages-eshell-buffer'."
  (let ((arg-list (append args (helm-marked-candidates)))
        (eshell-buffer-name helm-system-packages-eshell-buffer))
    ;; Refresh package list after command has completed.
    (add-hook 'eshell-post-command-hook 'helm-system-packages-refresh t)
    (push command arg-list)
    (push "sudo" arg-list)
    (eshell t)
    (insert (mapconcat 'identity arg-list " "))
    (eshell-send-input)))

(defun helm-system-packages-refresh (&optional lazy)
  (setq
   helm-system-packages--explicit
   (or (and (not lazy) helm-system-packages--explicit)
       (funcall helm-system-packages-list-explicit))
   helm-system-packages--dependencies
   (or (and (not lazy) helm-system-packages--dependencies)
       (funcall helm-system-packages-list-dependencies))
   helm-system-packages--all
   (or (and (not lazy) helm-system-packages--all)
       (funcall helm-system-packages-list-all))))

(defun helm-system-packages-init ()
  "Cache package lists and create Helm buffer."
  (helm-system-packages-refresh t)
  (unless (helm-candidate-buffer)
    (helm-init-candidates-in-buffer
        'global
      (with-temp-buffer
        (dolist (i helm-system-packages--all)
          (insert (concat i "\n")))
        (buffer-string)))))

;;;###autoload
(defun helm-system-packages ()
  "Helm user interface for system packages."
  (interactive)
  (cond
   ((executable-find "emerge")
    (require 'helm-system-packages-portage)
    (setq helm-system-packages-list-explicit 'helm-system-packages-portage-list-explicit
          helm-system-packages-list-dependencies 'helm-system-packages-portage-list-dependencies
          helm-system-packages-list-all 'helm-system-packages-portage-list-all)
    (helm-system-packages-portage))
   ((executable-find "dpkg")
    (require 'helm-system-packages-dpkg)
    (setq helm-system-packages-list-explicit 'helm-system-packages-dpkg-list-explicit
          helm-system-packages-list-dependencies 'helm-system-packages-dpkg-list-dependencies
          helm-system-packages-list-all 'helm-system-packages-dpkg-list-all)
    (helm-system-packages-dpkg))))

(provide 'helm-system-packages)

;;; helm-system-packages.el ends here

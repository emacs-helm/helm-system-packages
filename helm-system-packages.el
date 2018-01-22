;;; helm-system-packages.el --- Helm UI wrapper for system package managers. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017        Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.6.9
;; Package-Requires: ((emacs "24.4") (helm "2.8.6") (seq "1.8"))
;; Keywords: helm, packages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Helm UI wrapper for system package managers.

;;; Code:
(require 'seq)

(defvar helm-system-packages-eshell-buffer "*helm-system-packages-eshell*")
(defvar helm-system-packages-buffer "*helm-system-packages-output*")

;; TODO: Don't refresh when eshell-last-command-status is not 0?
(defvar helm-system-packages-refresh nil
  "Function to refresh the package list.
It is called:
- on each session start;
- whenever a shell command completes.")

;; TODO: Possible optimization: turn into hash table.
(defvar helm-system-packages--display-lists nil
  "List of (package . (faces...)).")

(defgroup helm-system-packages nil
  "Predefined configurations for `helm-system-packages'."
  :group 'helm)

(defcustom helm-system-packages-show-descriptions-p t
  "Always show descriptions in package list when non-nil."
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-auto-send-commandline-p t
  "When a transaction commandline is inserted into a shell buffer, "
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-candidate-limit 1000
  "Maximum number of candidates to display at once.

0 means display all."
  :group 'helm-system-packages
  :type 'integerp)

(defcustom helm-system-packages-use-symbol-at-point-p nil
  "Whether to use symbol at point as a default search entry."
  :group 'helm-system-packages
  :type 'boolean)

(defun helm-system-packages-toggle-descriptions ()
  "Toggle description column."
  (interactive)
  (with-helm-alive-p
  (setq helm-system-packages-show-descriptions-p (not helm-system-packages-show-descriptions-p))
    (helm-force-update)))
(put 'helm-system-packages-toggle-descriptions 'helm-only t)

;; TODO: Possible optimization: turn into macro.
(defun helm-system-packages-extract-name (package)
  "Extract package name from the candidate.
This is useful required because the Helm session runs over a buffer source, so
there is only a REAL value which might contain additional display information
such as the package description."
  (if helm-system-packages-show-descriptions-p
      (car (split-string package))
    package))

(defun helm-system-packages-run (command &rest args)
  "COMMAND to run over `helm-marked-candidates'."
  (let ((arg-list (append args (helm-marked-candidates))))
    (with-temp-buffer
      ;; We discard errors.
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
        ;; This is does not work for pacman which needs a specialized function.
        (setq res (replace-regexp-in-string "\\`.*: " "* " res))
        (setq res (replace-regexp-in-string "\n\n.*: " "\n* " res)))
      (insert res))))

(defun helm-system-packages-find-files (command &rest args)
  (require 'helm-files)
  (let ((res (apply #'helm-system-packages-run command args)))
    (if (string-empty-p res)
        (message "No result")
      (if helm-current-prefix-arg
          (insert res)
        (helm :sources (helm-build-sync-source "Package files"
                         :candidates (split-string res "\n")
                         :candidate-transformer (lambda (files)
                                                  (let ((helm-ff-transformer-show-only-basename nil))
                                                    (mapcar 'helm-ff-filter-candidate-one-by-one files)))
                         :candidate-number-limit 'helm-ff-candidate-number-limit
                         :persistent-action 'helm-find-files-persistent-action
                         :keymap 'helm-find-files-map
                         :action 'helm-find-files-actions)
              :buffer "*helm system package files*")))))

(defun helm-system-packages-run-as-root (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

COMMAND will be run in an Eshell buffer `helm-system-packages-eshell-buffer'."
  (let ((arg-list (append args (helm-marked-candidates)))
        (eshell-buffer-name helm-system-packages-eshell-buffer))
    ;; Refresh package list after command has completed.
    (push command arg-list)
    (push "sudo" arg-list)
    (eshell)
    (if (eshell-interactive-process)
        (message "A process is already running")
      (add-hook 'eshell-post-command-hook 'helm-system-packages-refresh nil t)
      (goto-char (point-max))
      (insert (mapconcat 'identity arg-list " "))
      (when helm-system-packages-auto-send-commandline-p
        (eshell-send-input)))))

(defun helm-system-packages-browse-url (urls)
  "Browse homepage URLs of `helm-marked-candidates'.

With prefix argument, insert the output at point."
  (cond
   ((not urls) (message "No result"))
   (helm-current-prefix-arg (insert urls))
   (t (mapc 'browse-url (helm-comp-read "URL: " urls :must-match t :exec-when-only-one t :marked-candidates t)))))

;;;###autoload
(defun helm-system-packages ()
  "Helm user interface for system packages."
  (interactive)
  (let ((managers (seq-filter 'executable-find '("emerge" "dpkg" "pacman"))))
    (if (not managers)
        (message "No supported package manager was found")
      (let ((manager (car managers)))
        (require (intern (concat "helm-system-packages-" manager)))
        (fset 'helm-system-packages-refresh (intern (concat "helm-system-packages-" manager "-refresh")))
        (funcall (intern (concat "helm-system-packages-" manager)))))))

(provide 'helm-system-packages)

;;; helm-system-packages.el ends here

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
(require 'helm-files)

(defvar helm-system-packages-eshell-buffer "*helm-system-packages-eshell*")
(defvar helm-system-packages-buffer "*helm-system-packages-output*")

(defvar helm-system-packages--explicit nil)
(defvar helm-system-packages--dependencies nil)
(defvar helm-system-packages--all nil)
(defvar helm-system-packages--descriptions nil)

(defface helm-system-packages-explicit '((t (:foreground "orange" :weight bold)))
  "Face for explicitly installed packages."
  :group 'helm-system-packages)

(defface helm-system-packages-dependencies '((t (:foreground "lightblue" :slant italic)))
  "Face for packages installed as dependencies."
  :group 'helm-system-packages)

(defgroup helm-system-packages nil
  "Predefined configurations for `helm-system-packages'."
  :group 'helm)

;; TODO: Add "C-]" to local map to toggle details.
(defcustom helm-system-packages-details-flag t
  "Always show details in package list when non-nil."
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-max-length 36 ; Seems to be a decent value for Portage.
  "Width of the package name column when displaying details."
  :group 'helm-system-packages
  :type 'integerp)

(defcustom helm-system-packages-candidate-limit 1000
  "Maximum number of candidates to display at once.

0 means display all."
  :group 'helm-system-packages
  :type 'integerp)

;; TODO: Match over description as well.
(defun helm-system-packages-highlight (packages)
  "Highlight all explicitly installed PACKAGES as well as dependencies."
  (mapcar (lambda (pkg)
            (let (display)
              (setq display
                    (propertize pkg 'face
                                (cond
                                 ((member pkg helm-system-packages--explicit) 'helm-system-packages-explicit)
                                 ((member pkg helm-system-packages--dependencies) 'helm-system-packages-dependencies)
                                 (t nil))))
              (when helm-system-packages-details-flag
                (setq display (concat
                               ;; TODO: Move this to cache instead?
                               (substring display 0 (min (length display) helm-system-packages-max-length))
                               (make-string (max (- helm-system-packages-max-length (length display)) 0) ? )
                               (or (and (> (length display) helm-system-packages-max-length) helm-buffers-end-truncated-string) " ")
                               " "
                               (alist-get (intern pkg) helm-system-packages--descriptions))))
              (cons display pkg)))
          packages))

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
        ;; TODO: This si too fragile and does not work for pacman.
        ;; Alternative: Simply replace the double linebreak with "* pkg".
        (setq res (replace-regexp-in-string "\\`.*: " "* " res))
        (setq res (replace-regexp-in-string "\n\n.*: " "\n* " res)))
      (insert res))))

(defun helm-system-packages-find-files (command &rest args)
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
      (eshell-send-input))))

(defun helm-system-packages-browse-url (urls)
  "Browse homepage URLs of `helm-marked-candidates'.

With prefix argument, insert the output at point."
  (cond
   ((not urls) (message "No result"))
   (helm-current-prefix-arg (insert urls))
   (t (mapc 'browse-url (helm-comp-read "URL: " urls :must-match t :exec-when-only-one t :marked-candidates t)))))

(defun helm-system-packages-refresh (&optional lazy)
  "Cache package lists.

There is no need to call this function unless the set of system
packages was changed externally, e.g. via a system upgrade.

With prefix argument or if LAZY is non-nil, only do it if the
lists have not already been set."
  (interactive "P")
  (dolist (cache '(helm-system-packages--explicit
                   helm-system-packages--dependencies
                   helm-system-packages--all
                   helm-system-packages--descriptions))
    (set cache (or (and lazy (symbol-value cache))
                   (and (functionp cache) (funcall cache))))))

(defun helm-system-packages-init ()
  "Cache package lists and create Helm buffer."
  (helm-system-packages-refresh t)
  (unless (helm-candidate-buffer)
    (helm-init-candidates-in-buffer
        'global
      helm-system-packages--all)))

;;;###autoload
(defun helm-system-packages ()
  "Helm user interface for system packages."
  (interactive)
  (let ((managers (seq-filter 'executable-find '("emerge" "dpkg" "pacman"))))
    (if (not managers)
        (message "No supported package manager was found")
      (let ((manager (car managers)))
        (require (intern (concat "helm-system-packages-" manager)))
        (fset 'helm-system-packages--explicit (intern (concat "helm-system-packages-" manager "-list-explicit")))
        (fset 'helm-system-packages--dependencies (intern (concat "helm-system-packages-" manager "-list-dependencies")))
        (fset 'helm-system-packages--all (intern (concat "helm-system-packages-" manager "-list-all")))
        (fset 'helm-system-packages--descriptions (intern (concat "helm-system-packages-" manager "-list-descriptions")))
        (funcall (intern (concat "helm-system-packages-" manager)))))))

(provide 'helm-system-packages)

;;; helm-system-packages.el ends here

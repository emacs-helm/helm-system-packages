;;; helm-system-packages.el --- Helm UI wrapper for system package managers. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.8.0
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

(defvar helm-system-packages--show-uninstalled-p t)
(defvar helm-system-packages--show-explicit-p t)
(defvar helm-system-packages--show-dependencies-p t)
(defvar helm-system-packages--show-orphans-p t)
(defvar helm-system-packages--show-locals-p t)
(defvar helm-system-packages--show-groups-p t)

(defvar helm-system-packages--source-name "package source")

(defvar helm-system-packages--names nil
  "Cache of all packages.")

(defvar helm-system-packages--descriptions nil
  "Cache of all package names with descriptions.")

(defface helm-system-packages-explicit '((t (:inherit font-lock-warning-face)))
  "Face for explicitly installed packages."
  :group 'helm-system-packages)

(defface helm-system-packages-dependencies '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for packages installed as dependencies."
  :group 'helm-system-packages)

(defface helm-system-packages-orphans '((t (:inherit font-lock-string-face :slant italic)))
  "Face for orphan packages (unrequired dependencies)."
  :group 'helm-system-packages)

(defface helm-system-packages-locals '((t (:weight bold)))
  "Face for local packages."
  :group 'helm-system-packages)

(defface helm-system-packages-groups '((t (:inherit font-lock-doc-face)))
  "Face for package groups."
  :group 'helm-system-packages)

(defface helm-system-packages-virtual '((t (:slant italic)))
  "Face for virtual packages."
  :group 'helm-system-packages)

;; Shut up byte compiler
(declare-function eshell-interactive-process "esh-cmd.el")
(declare-function eshell-send-input "esh-mode.el")
(defvar eshell-buffer-name)
(defvar helm-ff-transformer-show-only-basename)
(declare-function helm-comp-read "helm-mode.el")
(declare-function org-sort-entries "org.el")

(defun helm-system-packages-toggle-explicit ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-explicit-p (not helm-system-packages--show-explicit-p))
    (helm-update)))
(put 'helm-system-packages-toggle-explicit 'helm-only t)

(defun helm-system-packages-toggle-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-uninstalled-p (not helm-system-packages--show-uninstalled-p))
    (helm-update)))
(put 'helm-system-packages-toggle-uninstalled 'helm-only t)

(defun helm-system-packages-toggle-dependencies ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-dependencies-p (not helm-system-packages--show-dependencies-p))
    (helm-update)))
(put 'helm-system-packages-toggle-dependencies 'helm-only t)

(defun helm-system-packages-toggle-orphans ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-orphans-p (not helm-system-packages--show-orphans-p))
    (helm-update)))
(put 'helm-system-packages-toggle-orphans 'helm-only t)

(defun helm-system-packages-toggle-locals ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-locals-p (not helm-system-packages--show-locals-p))
    (helm-update)))
(put 'helm-system-packages-toggle-locals 'helm-only t)

(defun helm-system-packages-toggle-groups ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-groups-p (not helm-system-packages--show-groups-p))
    (helm-update)))
(put 'helm-system-packages-toggle-groups 'helm-only t)

;; TODO: Don't refresh when eshell-last-command-status is not 0?
(defvar helm-system-packages-refresh nil
  "Function to refresh the package list.
It is called:
- on each session start;
- whenever a shell command completes.")

;; TODO: Possible optimization: turn into hash table.
;; TODO: This is an implementation detail: move to every package interface that needs it?
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
  :type 'integer)

(defcustom helm-system-packages-use-symbol-at-point-p nil
  "Whether to use symbol at point as a default search entry."
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-editable-info-p nil
  "Whether info buffer is editable.
If nil, it is displayed in view-mode, which means \"q\" (default binding) quits
the window."
  :group 'helm-system-packages
  :type 'boolean)

(defun helm-system-packages-mapalist (fun-alist alist)
  "Apply the FUN-ALIST function to each element in ALIST with the same key.
Return the alist of the results.
Keys must be symbols.
The special key `all' matches all members in ALIST.
Only the first match is applied.
If a member of ALIST does not have a matching function, it is dropped.

To explicitly drop an element, use the `ignore' function.
To explicitly keep an element, use the `identity' function."
  (let ((fun-all (car (alist-get 'all fun-alist)))
        (result '()))
    (dolist (e alist)
      (let* ((fun (or (car (alist-get (car e) fun-alist))
                      fun-all))
             (res (and fun (apply fun (cdr e)))))
        (when res
          (push (list (car e) res) result))))
    (nreverse result)))

(defun helm-system-packages-categorize (packages)
  "Return an alist of PACKAGES indexed by category.
PACKAGES must be a list.
Order is presever within categories.
Categories are infered from `helm-system-packages--display-lists': it's the last
word of the first associated symbol.
If not found, category is `uninstalled'."
  (let ((result '())) ; TODO: Include in dolist.
    (dolist (p packages)
      (let* ((e (assoc p helm-system-packages--display-lists))
             (category (or (and e (intern (replace-regexp-in-string ".*\\W\\(\\w+\\)$" "\\1" (symbol-name (cadr e)))))
                           'uninstalled))
             (cell (assoc category result)))
        (if cell
            (setcdr cell (nconc (cdr cell) (list p)))
          (push (list category p) result))))
    result))

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

(defun helm-system-packages-show-information (desc-alist)
  "Show package information contained in DESC-ALIST.
DESC-ALIST's cars are ignored, the cdrs are in the form

.*: PACKAGE-NAME
PACKAGE-INFO...

.*: OTHER-PACKAGE-NAME
..."
  (cond
   ((not desc-alist)
    (message "No information for package(s) %s" (mapconcat 'identity (helm-marked-candidates) " ")))
   ;; TODO: Sort buffer output? Or keep the mark order?
   (helm-current-prefix-arg
    (mapc 'insert (mapcar 'cadr desc-alist)))
   (t (switch-to-buffer helm-system-packages-buffer)
      (view-mode 0)
      (erase-buffer)
      (insert "\n\n")
      (mapc 'insert (mapcar 'cadr desc-alist))
      (goto-char (point-min))
      (while (re-search-forward "\n\n.*: " nil t)
        (replace-match "\n* "))
      (goto-char (point-min))
      (org-mode)
      (org-sort-entries nil ?a)
      (goto-char (point-min))
      (delete-blank-lines)
      (unless (or helm-current-prefix-arg helm-system-packages-editable-info-p)
        (view-mode 1)))))

;; TODO: If we do not make 'args' a &rest, then `apply' can be removed in the caller.
(defun helm-system-packages-call (commandline &rest args)
  "COMMANDLINE to run over ARGS.
COMMANDLINE is a list where the `car' is the command and the
`cdr' are the options."
  (with-temp-buffer
    ;; We discard errors.
    (apply #'call-process (car commandline) nil t nil (append (cdr commandline) args))
    (buffer-string)))

;; TODO: Replace -run by -call.
(defun helm-system-packages-run (command &rest args)
  "COMMAND to run over `helm-marked-candidates'."
  (let ((arg-list (append args (helm-marked-candidates))))
    (with-temp-buffer
      ;; We discard errors.
      (apply #'call-process command nil t nil arg-list)
      (buffer-string))))

;; TODO: Replace -print by -show-information.
(defun helm-system-packages-print (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (let ((res (apply #'helm-system-packages-run command args)))
    (if (string= res "")
        (message "No result")
      (unless helm-current-prefix-arg
        (switch-to-buffer helm-system-packages-buffer)
        (view-mode 0)
        (erase-buffer)
        (org-mode)
        ;; This is does not work for pacman which needs a specialized function.
        (setq res (replace-regexp-in-string "\\`.*: " "* " res))
        (setq res (replace-regexp-in-string "\n\n.*: " "\n* " res)))
      (save-excursion (insert res))
      (unless (or helm-current-prefix-arg helm-system-packages-editable-info-p)
        (view-mode 1)))))

(defun helm-system-packages-build-file-source (package files)
  "Build Helm file source for PACKAGE with FILES candidates.
PACKAGES is a string and FILES is a list of strings."
  (require 'helm-files)
  (helm-build-sync-source (concat package " files")
    :candidates files
    :candidate-transformer (lambda (files)
                             (let ((helm-ff-transformer-show-only-basename nil))
                               (mapcar 'helm-ff-filter-candidate-one-by-one files)))
    :candidate-number-limit 'helm-ff-candidate-number-limit
    :persistent-action-if 'helm-find-files-persistent-action-if
    :keymap 'helm-find-files-map
    :action 'helm-find-files-actions))

;; TODO: Replace by -pacman-find-files.
(defun helm-system-packages-find-files (command &rest args)
  (let ((res (apply #'helm-system-packages-run command args)))
    (if (string= res "")
        (message "No result") ; TODO: Error in helm-system-packages-run.
      (if helm-current-prefix-arg
          (insert res)
        (helm :sources
              (helm-system-packages-build-file-source "Packages" (split-string res "\n"))
              :buffer "*helm system package files*")))))

(defun helm-system-packages-run-as-root (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

COMMAND will be run in an Eshell buffer `helm-system-packages-eshell-buffer'."
  (require 'esh-mode)
  (let ((arg-list (append args (helm-marked-candidates)))
        (eshell-buffer-name helm-system-packages-eshell-buffer))
    ;; Refresh package list after command has completed.
    (push command arg-list)
    (push "sudo" arg-list)
    (eshell)
    (if (eshell-interactive-process)
        (message "A process is already running")
      (add-hook 'eshell-post-command-hook 'helm-system-packages-refresh nil t)
      (add-hook 'eshell-post-command-hook
                (lambda () (remove-hook 'eshell-post-command-hook 'helm-system-packages-refresh t))
                t t)
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

(defun helm-system-packages-missing-dependencies-p (&rest deps)
  "Return non-nil if some DEPS are missing."
  (let ((missing-deps (delq nil (mapcar
                                 (lambda (exe) (if (not (executable-find exe)) exe))
                                 deps))))
    (when missing-deps
      (message "Dependencies are missing (%s), please install them"
               (mapconcat 'identity missing-deps ", ")))))

;;;###autoload
(defun helm-system-packages ()
  "Helm user interface for system packages."
  (interactive)
  ;; "portage" does not have an executable of the same name, hence the optional pair (PACKAGE-MANAGER EXECUTABLE).
  (let ((managers (seq-filter (lambda (p) (executable-find (car p))) '(("emerge" "portage") ("dpkg") ("pacman")))))
    (if (not managers)
        (message "No supported package manager was found")
      (let ((manager (car (last (car managers)))))
        (require (intern (concat "helm-system-packages-" manager)))
        (fset 'helm-system-packages-refresh (intern (concat "helm-system-packages-" manager "-refresh")))
        (funcall (intern (concat "helm-system-packages-" manager)))))))

(provide 'helm-system-packages)

;;; helm-system-packages.el ends here

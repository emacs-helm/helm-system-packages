;;; helm-system-packages-xbps.el --- Helm UI for Void Linux' xbps. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.8.0
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
;; Helm UI for Void Linux' xbps.

;;; Code:
(require 'helm)
(require 'helm-system-packages)

;; Shut up byte compiler
(declare-function eshell-interactive-process "esh-cmd.el")

(defvar helm-system-packages-xbps-help-message
  "* Helm xbps

** Options

- `helm-system-packages-xbps-confirm-p'
- `helm-system-packages-xbps-sync-threshold'
- `helm-system-packages-xbps-auto-clean-cache'

** Commands
\\<helm-system-packages-xbps-map>
\\[helm-system-packages-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-toggle-dependencies]\t\tToggle display of required dependencies.
\\[helm-system-packages-toggle-orphans]\t\tToggle display of unrequired dependencies.
\\[helm-system-packages-toggle-pinned]\t\tToggle display of pinned packages.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-xbps-map
  ;; M-U is reserved for `helm-unmark-all'.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-toggle-explicit)
    (define-key map (kbd "M-N")   'helm-system-packages-toggle-uninstalled)
    (define-key map (kbd "M-D")   'helm-system-packages-toggle-dependencies)
    (define-key map (kbd "M-O")   'helm-system-packages-toggle-orphans)
    (define-key map (kbd "M-P")   'helm-system-packages-toggle-pinned)
    (define-key map (kbd "C-]")   'helm-system-packages-toggle-descriptions)
    map))

(defun helm-system-packages-xbps-transformer (packages)
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p) helm-system-packages--display-lists))))
        (cond
         ((and (not face) helm-system-packages--show-uninstalled-p)
          (push p res))
         ((or
           (and helm-system-packages--show-explicit-p (memq 'helm-system-packages-explicit face))
           (and helm-system-packages--show-dependencies-p (memq 'helm-system-packages-dependencies face))
           (and helm-system-packages--show-orphans-p (memq 'helm-system-packages-orphans face))
           (and helm-system-packages--show-pinned-p (memq 'helm-system-packages-pinned face)))
          (push (propertize p 'face (car face)) res)))))))

(defun helm-system-packages-xbps-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (call-process "xbps-query" nil t nil "--list-manual-pkgs")
                  (goto-char (point-min))
                  (while (re-search-forward "-[^-]+$" nil t)
                    (replace-match ""))
                  (buffer-string))))

(defun helm-system-packages-xbps-list-dependencies (&rest non-dependencies)
  "List packages installed as a required dependency.
NON-DEPENDENCIES are package lists which are to be excluded."
  (seq-reduce
   'seq-difference non-dependencies
   (split-string (with-temp-buffer
                   ;; --property automatic-install is always true... Is it a bug?
                   ;; Thus we need to subtract all other installed packages categories.,
                   (call-process "xbps-query" nil t nil "--search" "" "--prop" "automatic-install")
                   (goto-char (point-min))
                   (while (re-search-forward "-[^-]+$" nil t)
                     (replace-match ""))
                   (buffer-string)))))

(defun helm-system-packages-xbps-list-orphans ()
  "List orphan packages (unrequired dependencies)."
  (split-string (with-temp-buffer
                  (call-process "xbps-query" nil t nil "--list-orphans")
                  (goto-char (point-min))
                  (while (re-search-forward "-[^-]+$" nil t)
                    (replace-match ""))
                  (buffer-string))))

(defun helm-system-packages-xbps-list-pinned ()
  "List pinned installed packages.
That is, packages that won't be updated automatically."
  (split-string (with-temp-buffer
                  (call-process "xbps-query" nil t nil "--list-hold-pkgs")
                  (goto-char (point-min))
                  (while (re-search-forward "-[^-]+$" nil t)
                    (replace-match ""))
                  (buffer-string))))

(defun helm-system-packages-xbps-cache ()
  "Cache all package names with descriptions.
Return (NAMES . DESCRIPTIONS), a cons of two strings."
  ;; We build both caches at the same time.  We could also build just-in-time, but
  ;; benchmarks show that it only saves less than 20% when building one cache.
  (let (names descriptions)
    (setq descriptions
          (with-temp-buffer
            (call-process "xbps-query" nil t nil "-R" "--search" "")
            (goto-char (point-min))
            (while (not (eobp))
              (delete-char 4)
              (search-forward " " nil t)
              (while (not (char-equal (char-after) ?-))
                ;; Replace version with whitespace.
                (delete-char 1)
                (insert " ")
                (backward-char 2))
              (delete-char 1)
              (insert " ")
              (forward-line))
            (buffer-string)))
    ;; replace-regexp-in-string is faster than mapconcat over split-string.
    (setq names
          (replace-regexp-in-string " .*" "" descriptions))
    (cons names descriptions)))

(defun helm-system-packages-xbps-refresh ()
  "Refresh the package list."
  (interactive)
  (setq helm-system-packages--source-name "xbps source")
  (let* ((explicit (helm-system-packages-xbps-list-explicit))
         (orphans (helm-system-packages-xbps-list-orphans))
         (pinned (helm-system-packages-xbps-list-pinned))
         (dependencies (helm-system-packages-xbps-list-dependencies explicit orphans pinned)))
    (let ((res (helm-system-packages-xbps-cache)))
      (setq helm-system-packages--names (car res)
            helm-system-packages--descriptions (cdr res)))
    (setq helm-system-packages--display-lists nil)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-explicit)) helm-system-packages--display-lists))
    (dolist (p dependencies)
      (push (cons p '(helm-system-packages-dependencies)) helm-system-packages--display-lists))
    (dolist (p orphans)
      (push (cons p '(helm-system-packages-orphans)) helm-system-packages--display-lists))
    (dolist (p pinned)
      (push (cons p '(helm-system-packages-pinned)) helm-system-packages--display-lists))))

(defun helm-system-packages-xbps-init ()
  "Cache package lists and create Helm buffer."
  (unless (and helm-system-packages--names helm-system-packages--descriptions)
    (helm-system-packages-xbps-refresh))
  (helm-init-candidates-in-buffer
      'global
    (if helm-system-packages-show-descriptions-p
        helm-system-packages--descriptions
      helm-system-packages--names)))

(defcustom helm-system-packages-xbps-synchronize-threshold 86400
  "Auto-synchronize database on installation if older than this many seconds.
If nil, no automatic action is taken."
  :group 'helm-system-packages
  :type 'integer)

(defun helm-system-packages-xbps-outdated-database-p ()
  "Return non-nil when database is older than `helm-system-packages-pacman-synchronize-threshold'."
  (when helm-system-packages-xbps-synchronize-threshold
    ;; Check the date of the youngest database.
    (time-less-p (car (nreverse
                       (sort (mapcar
                              (lambda (file) (nth 5 (file-attributes file)))
                              (file-expand-wildcards (expand-file-name "/var/db/xbps/*/*-repodata")))
                             'time-less-p)))
                 (time-subtract (current-time) (seconds-to-time helm-system-packages-xbps-synchronize-threshold)))))

(defun helm-system-packages-xbps-info (_candidate)
  "Print information about the selected packages.

The local database will be queried if possible, while the sync
database is used as a fallback.  Note that they don't hold the
exact same information.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (helm-system-packages-show-information
   (helm-system-packages-mapalist
    '((uninstalled
       (lambda (packages)
         ;; It seems that xbps-query cannot work out multiple packages at once.
         (mapcar (lambda (pkg)
                   (cons pkg
                         (helm-system-packages-call "xbps-query" nil "-R" pkg)))
                 packages)))
      (all (lambda (packages)
             (mapcar (lambda (pkg)
                       (cons pkg
                             (helm-system-packages-call "xbps-query" nil pkg)))
                     packages))))
    (helm-system-packages-categorize (helm-marked-candidates)))))

(defcustom helm-system-packages-xbps-auto-clean-cache nil
  "Clean cache before installing.
The point of keeping previous version in cache is that you can revert back if
something fails.
By always cleaning before installing, the previous version in kept in cache.
This is only healthy if you test every version you install.
Installing two upgrades (or the same version) will effectively leave you with no
tested package to fall back on."
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-xbps-confirm-p t
  "Prompt for confirmation before proceeding with transaction."
  :group 'helm-system-packages
  :type 'boolean)

(defun helm-system-packages-xbps-install (_)
  "Install marked candidates."
  (when helm-system-packages-xbps-auto-clean-cache
    (let ((eshell-buffer-name helm-system-packages-eshell-buffer))
      (eshell)
      (unless (eshell-interactive-process)
        (goto-char (point-max))
        (insert "sudo xbps-remove --clean-cache "
                "&& "))))
  (helm-system-packages-run-as-root
   "xbps-install"
   (when (helm-system-packages-xbps-outdated-database-p) "--sync")
   (when helm-current-prefix-arg "--force")
   (unless helm-system-packages-xbps-confirm-p "--yes")))

(defun helm-system-packages-xbps-uninstall (_)
  "Uninstall marked candidates."
  (helm-system-packages-run-as-root-over-installed
   "xbps-remove"
   (when helm-current-prefix-arg "--recursive")
   (unless helm-system-packages-xbps-confirm-p "--yes")))

(defun helm-system-packages-xbps-browse-url (_)
  "Browse homepage URLs of marked candidates."
  (helm-system-packages-browse-url
   (mapcar (lambda (pkg)
             (helm-system-packages-call "xbps-query" nil "-R" "--prop" "homepage" pkg))
           (helm-marked-candidates))))

(defun helm-system-packages-xbps-find-files (_)
  "Find files for marked candidates."
  (helm-system-packages-find-files
   (let ((file-hash (make-hash-table :test 'equal)))
     (dolist (pkg (helm-marked-candidates) file-hash)
       (dolist (file (split-string
                      ;; Remove " -> /link/indirection" from output.
                      (replace-regexp-in-string
                       " -> .*" ""
                       (helm-system-packages-call "xbps-query" nil "-R" "--files" pkg))
                      "\n" t))
         (push file (gethash pkg file-hash)))))))

(defun helm-system-packages-xbps-show-dependencies (_candidate &optional reverse)
  "List candidate dependencies for `helm-system-packages-show-packages'.
If REVERSE is non-nil, list reverse dependencies instead."
  (let ((arg (if reverse "-X" "-x"))
        (helm-system-packages--source-name (concat
                                            (if reverse "Reverse dependencies" "Dependencies")
                                            " of "
                                            (mapconcat 'identity (helm-marked-candidates) " "))))
    (helm-system-packages-show-packages
     `((uninstalled
        ,(mapconcat 'identity
                    (mapcar (lambda (pkg)
                              ;; Remove version numbers.
                              (replace-regexp-in-string
                               "[-<>][^-<>]+$" ""
                               (helm-system-packages-call "xbps-query" nil "-R" arg pkg)))
                            (helm-marked-candidates))
                    "\n"))))))

(defcustom helm-system-packages-xbps-actions
  '(("Show package(s)" . helm-system-packages-xbps-info)
    ("Install (`C-u' to reinstall)" . helm-system-packages-xbps-install)
    ("Uninstall (`C-u' to include dependencies)" . helm-system-packages-xbps-uninstall)
    ("Browse homepage URL" . helm-system-packages-xbps-browse-url)
    ("Find files" . helm-system-packages-xbps-find-files)
    ("Show dependencies (`C-u' to include optional deps)" . helm-system-packages-xbps-show-dependencies)
    ("Show reverse dependencies" .
     (lambda (_)
       (helm-system-packages-xbps-show-dependencies _ 'reverse)))
    ("Mark as dependency" .
     (lambda (_)
       (helm-system-packages-run-as-root-over-installed "xbps-pkgdb" "--mode" "auto")))
    ("Mark as explicit" .
     (lambda (_)
       (helm-system-packages-run-as-root-over-installed "xbps-pkgdb" "--mode" "manual")))
    ("Pin package" .
     (lambda (_)
       (helm-system-packages-run-as-root-over-installed "xbps-pkgdb" "--mode" "hold")))
    ("Unpin package" .
     (lambda (_)
       (helm-system-packages-run-as-root-over-installed "xbps-pkgdb" "--mode" "unhold")))
    ("Check for errors" .
     (lambda (_)
       (helm-system-packages-run-as-root-over-installed "xbps-pkgdb")))
    ("Reconfigure (`C-u' to force)" .
     (lambda (_)
       (helm-system-packages-run-as-root-over-installed
        "xbps-reconfigure"
        (when helm-current-prefix-arg "--force")))))
  "Actions for Helm xbps."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-xbps-build-source ()
  "Build Helm source for xbps."
  (helm-build-in-buffer-source helm-system-packages--source-name
    :init 'helm-system-packages-xbps-init
    :candidate-transformer 'helm-system-packages-xbps-transformer
    :candidate-number-limit helm-system-packages-candidate-limit
    :display-to-real 'helm-system-packages-extract-name
    :keymap helm-system-packages-xbps-map
    :help-message 'helm-system-packages-xbps-help-message
    :persistent-help "Show package description"
    :action helm-system-packages-xbps-actions))

(defun helm-system-packages-xbps ()
  "Preconfigured `helm' for xbps."
  (helm :sources (helm-system-packages-xbps-build-source)
        :buffer "*helm xbps*"
        :truncate-lines t
        :input (when helm-system-packages-use-symbol-at-point-p
                 (substring-no-properties (or (thing-at-point 'symbol) "")))))

(provide 'helm-system-packages-xbps)

;;; helm-system-packages-xbps.el ends here

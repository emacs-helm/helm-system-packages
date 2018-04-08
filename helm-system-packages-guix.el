;;; helm-system-packages-guix.el --- Helm UI for Guix. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.9.0
;; Package-Requires: ((emacs "25") (helm "2.8.6"))
;; Keywords: helm, guix, packages

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
;; Helm UI for Guix.

;;; Code:
(require 'helm)
(require 'helm-system-packages)

;; TODO: Add support for superseded and obsolete packages.
;; TODO: Add support for multiple outputs (install, uninstall, listing...).
;; TODO: Add support for multiple versions.
;; TODO: Use guix.el instead of parsing guix commandline output.
;; TODO: Refresh the cache automatically when a `guix pull' has happened.

(defvar helm-system-packages-guix-help-message
  "* Helm guix

Requirements:

** Commands
\\<helm-system-packages-guix-map>
\\[helm-system-packages-guix-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-guix-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-guix-map
  ;; M-U is reserved for `helm-unmark-all'.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-guix-toggle-explicit)
    (define-key map (kbd "M-N")   'helm-system-packages-guix-toggle-uninstalled)
    (define-key map (kbd "C-]")   'helm-system-packages-toggle-descriptions)
    map))

(defvar helm-system-packages-guix--show-uninstalled-p t)
(defvar helm-system-packages-guix--show-explicit-p t)

(defun helm-system-packages-guix-toggle-explicit ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-guix--show-explicit-p
          (not helm-system-packages-guix--show-explicit-p))
    (helm-update)))
(put 'helm-system-packages-guix-toggle-explicit 'helm-only t)

(defun helm-system-packages-guix-toggle-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-guix--show-uninstalled-p
          (not helm-system-packages-guix--show-uninstalled-p))
    (helm-update)))
(put 'helm-system-packages-guix-toggle-uninstalled 'helm-only t)

(defun helm-system-packages-guix-transformer (packages)
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p)
                              (plist-get (helm-system-packages--cache-get) :display)))))
        (cond
         ((not face)
          (when helm-system-packages-guix--show-uninstalled-p
            (push p res)))
         ((and helm-system-packages-guix--show-explicit-p
               (memq 'helm-system-packages-explicit face))
          (push (propertize p 'face (car face)) res)))))))

(defun helm-system-packages-guix-list-explicit ()
  "List explicitly installed packages."
  (split-string
   (with-temp-buffer
     (process-file-shell-command "guix package --list-installed | cut -f1" nil t)
     (sort-lines nil (point-min) (point-max))
     (buffer-string))))

(defvar helm-system-packages-guix-cache-file
  (expand-file-name "helm-system-packages-guix" user-emacs-directory)
  "Filename of the cache storing all Guix package descriptions.")

(defun helm-system-packages-guix-cache-file-get ()
  "Return Guix local cache.
If `default-directory' is a remote file (over TRAMP), a different
cache filename is returned with the host name appended to it."
  (concat helm-system-packages-guix-cache-file
          (when (tramp-tramp-file-p default-directory)
            (concat "_" (tramp-file-name-host
                         (tramp-dissect-file-name default-directory))))
          ".cache"))

;; Guix is extremely slow so we cache all entries once on drive.
(defun helm-system-packages-guix-cache (display-list &optional refresh-cache)
  "Cache all package names with descriptions."
  ;; We build both caches at the same time.  We could also build just-in-time, but
  ;; benchmarks show that it only saves less than 20% when building one cache.
  (let* (names
         descriptions
         (cache-file-name (helm-system-packages-guix-cache-file-get)))
    (when (or (not (file-exists-p cache-file-name))
              refresh-cache)
      (process-file "guix" nil `(:file ,cache-file-name) nil "package" "--search=."))
    (setq descriptions
          (with-temp-buffer
            (process-file "recsel" (helm-system-packages-guix-cache-file-get) t nil "-R" "name,synopsis")
            (goto-char (point-min))
            (while (search-forward " " nil t)
              (delete-char -1)
              (let ((pos (- (point) (line-beginning-position))))
                (when (< pos helm-system-packages-column-width)
                  (insert (make-string (- helm-system-packages-column-width pos) ? ))))
              (forward-line))
            (sort-lines nil (point-min) (point-max))
            (goto-char (point-min))
            (delete-blank-lines)
            (delete-blank-lines)
            (buffer-string)))
    ;; replace-regexp-in-string is faster than mapconcat over split-string.
    (setq names
          (replace-regexp-in-string " .*" "" descriptions))
    (helm-system-packages--cache-set names descriptions display-list "guix")))

(defun helm-system-packages-guix-refresh (&optional refresh-cache)
  "Refresh the list of installed packages.
With prefix argument or when `refresh-cache' is non-nil, refresh the cache."
  (interactive "P")
  (let* ((explicit (helm-system-packages-guix-list-explicit))
         display-list)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-explicit)) display-list))
    (helm-system-packages-guix-cache display-list refresh-cache)))

(defun helm-system-packages-guix-info (candidate)
  "Print information about the selected packages.
With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (helm-system-packages-show-information
   `((uninstalled . ,(mapcar (lambda (pkg-desc)
                               (let (name desc)
                                 (with-temp-buffer
                                   (insert pkg-desc)
                                   (goto-char (point-min))
                                   (search-forward ":" nil t)
                                   (setq name (buffer-substring-no-properties (point) (line-end-position)))
                                   (forward-line)
                                   (setq desc (buffer-substring-no-properties (point) (point-max)))
                                   (cons name desc))))
                             (split-string
                              (helm-system-packages-call
                               "recsel" nil "-e"
                               (mapconcat (lambda (s) (format "name = '%s'" s))
                                          (if helm-in-persistent-action
                                              (list candidate)
                                            (helm-marked-candidates))
                                          "||")
                               (helm-system-packages-guix-cache-file-get))
                              "\n\n"))))))

(defun helm-system-packages-guix-run (command args packages)
  "Call COMMAND ARGS PACKAGES as current user (sudo is not used).
ARGS and PACKAGES must be lists.
COMMAND will be run in the Eshell buffer named by `helm-system-packages-shell-name'."
  (require 'esh-mode)
  (if (not packages)
      (message "No suitable package selected")
    (let ((arg-list (append args packages))
          (eshell-buffer-name (helm-system-packages-shell-name)))
      ;; Refresh package list after command has completed.
      (eshell)
      (if (eshell-interactive-process)
          (message "A process is already running")
        (push command arg-list)
        (add-hook 'eshell-post-command-hook 'helm-system-packages-refresh nil t)
        (add-hook 'eshell-post-command-hook
                  (lambda () (remove-hook 'eshell-post-command-hook 'helm-system-packages-refresh t))
                  t t)
        (goto-char (point-max))
        (insert (mapconcat 'identity arg-list " "))
        (when helm-system-packages-auto-send-commandline-p
          (eshell-send-input))))))

(defun helm-system-packages-guix-install (_)
  "Install marked candidates."
  (helm-system-packages-guix-run
   "guix" '("package" "--install")
   (helm-marked-candidates)))

(defun helm-system-packages-guix-uninstall (_)
  "Install marked candidates."
  (helm-system-packages-guix-run
   "guix" '("package" "--remove")
   (helm-marked-candidates)))

(defun helm-system-packages-guix-browse-url (_)
  "Print homepage URLs of `helm-marked-candidates'.
With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (helm-system-packages-browse-url
   (split-string
    (helm-system-packages-call
     "recsel" nil "-R" "homepage" "-e"
     (mapconcat (lambda (s) (format "name = '%s'" s))
                (helm-marked-candidates)
                "||")
     (helm-system-packages-guix-cache-file-get))
    "\n" t)))

(defun helm-system-packages-guix-find-files (_)
  "Find files for marked candidates."
  (helm-system-packages-find-files
   (let* ((file-hash (make-hash-table :test 'equal))
          (package-hash (make-hash-table :test 'equal)))
     (dolist (s (split-string
                 (helm-system-packages-call "guix" nil "package" "--list-installed")
                 "\n" t))
       (let ((l (split-string s "\t")))
         (push (nth 3 l) (gethash (car l) package-hash))))
     (dolist (pkg (helm-marked-candidates) file-hash)
       (let ((h (gethash pkg package-hash)))
         (when h
           (setf (gethash pkg file-hash) (directory-files-recursively (car h) "."))))))))

;; TODO: -show-dependencies does not work with package names with 2 fields for
;; version numbers, e.g. ncurses-6.0-20170930.
(defun helm-system-packages-guix-show-dependencies (_candidate)
  "List candidate dependencies for `helm-system-packages-show-packages'. "
  (let ((title (concat
                "Dependencies of "
                (mapconcat 'identity (helm-marked-candidates) " "))))
    (helm-system-packages-show-packages
     `((uninstalled . ,(replace-regexp-in-string
                        "-[^-]+\n" "\n"
                        (replace-regexp-in-string
                         " " "\n"
                         (helm-system-packages-call
                          "recsel" nil "-R" "dependencies" "-e"
                          (mapconcat (lambda (s) (format "name = '%s'" s))
                                     (helm-marked-candidates)
                                     "||")
                          (helm-system-packages-guix-cache-file-get))))))
     title)))

(defun helm-system-packages-guix-show-reverse-dependencies (_candidate)
  "List candidate reverse dependencies for `helm-system-packages-show-packages'. "
  (let ((title (concat
                "Reverse dependencies of "
                (mapconcat 'identity (helm-marked-candidates) " "))))
    (helm-system-packages-show-packages
     `((uninstalled . ,(with-temp-buffer
                         (insert
                          (replace-regexp-in-string
                           " " "\n"
                           (replace-regexp-in-string
                            "@[^ ]+" ""
                            (replace-regexp-in-string
                             ".*: " ""
                             (helm-system-packages-call
                              "guix" (helm-marked-candidates) "refresh" "--list-dependent")))))
                         (sort-lines nil (point-min) (point-max))
                         (buffer-string))))
     title)))

(defcustom helm-system-packages-guix-actions
  '(("Show package(s)" . helm-system-packages-guix-info)
    ("Install" . helm-system-packages-guix-install)
    ("Uninstall" . helm-system-packages-guix-uninstall)
    ("Browse homepage URL" . helm-system-packages-guix-browse-url)
    ("Find files" . helm-system-packages-guix-find-files)
    ("Show dependencies" . helm-system-packages-guix-show-dependencies)
    ("Show reverse dependencies" . helm-system-packages-guix-show-reverse-dependencies))
  "Actions for Helm guix."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-guix-build-source ()
  "Build Helm source for guix"
  (let ((title (or (plist-get (helm-system-packages--cache-get) :title) "package manager")))
    (helm-build-in-buffer-source title
      :init 'helm-system-packages-init
      :candidate-transformer 'helm-system-packages-guix-transformer
      :candidate-number-limit helm-system-packages-candidate-limit
      :display-to-real 'helm-system-packages-extract-name
      :keymap helm-system-packages-guix-map
      :help-message 'helm-system-packages-guix-help-message
      :persistent-help "Show package description"
      :action helm-system-packages-guix-actions)))

(defun helm-system-packages-guix ()
  "Preconfigured `helm' for guix."
  ;; Guix can be installed beside another package manager.  Let's make this
  ;; command directly accessible then so that both the original package manager
  ;; and Guix can be called.
  (interactive)
  (unless (helm-system-packages-missing-dependencies-p "guix" "recsel")
    (helm :sources (helm-system-packages-guix-build-source)
          :buffer "*helm guix*"
          :truncate-lines t
          :input (when helm-system-packages-use-symbol-at-point-p
                   (substring-no-properties (or (thing-at-point 'symbol) ""))))))

(provide 'helm-system-packages-guix)

;;; helm-system-packages-guix.el ends here

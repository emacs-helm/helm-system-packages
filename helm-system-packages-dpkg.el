;;; helm-system-packages-dpkg.el --- Helm UI for Debian's dpkg. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.7.0
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
;; Helm UI for dpkg.

;;; Code:
(require 'helm)
(require 'helm-system-packages)

(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")

(defvar helm-system-packages-dpkg-help-message
  "* Helm dpkg

Requirements:

- dpkg
- apt-get
- apt-cache
- apt-mark

** Commands
\\<helm-system-packages-dpkg-map>
\\[helm-system-packages-dpkg-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-dpkg-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-dpkg-toggle-dependencies]\t\tToggle display of dependencies.
\\[helm-system-packages-dpkg-toggle-residuals]\t\tToggle display of package with residual configuration files.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-dpkg--names nil
  "Cache of all package names.")

(defvar helm-system-packages-dpkg--descriptions nil
  "Cache of all package names with descriptions.")

(defvar helm-system-packages-dpkg-term-buffer nil)

(defun helm-system-packages-dpkg-run-as-root (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

COMMAND will be run in a term-mode buffer
`helm-system-packages-dpkg-term-buffer'.  Eshell cannot be used
for dpkg because of its \"configure\" mecanism which uses a
curses interface."
  (let ((arg-list (append args (helm-marked-candidates))))
    ;; Refresh package list after command has completed.
    (push command arg-list)
    (push "sudo" arg-list)
    (if (and helm-system-packages-dpkg-term-buffer
             (buffer-live-p (get-buffer helm-system-packages-dpkg-term-buffer)))
        (switch-to-buffer helm-system-packages-dpkg-term-buffer)
      (ansi-term (getenv "SHELL") "term dpkg")
      (setq helm-system-packages-dpkg-term-buffer (buffer-name))
      (term-line-mode))
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    ;; TODO: Detect if an existing process is still running.
    (delete-region (point) (point-max))
    (insert (mapconcat 'identity arg-list " "))
    (term-char-mode)
    ;; It seems that it's not possible to add a post-command hook with "term".
    ;; Let's reset the cache right-away then.
    (setq helm-system-packages-dpkg--names nil
          helm-system-packages-dpkg--descriptions nil)
    (when helm-system-packages-auto-send-commandline-p
      (term-send-input))))

(defvar helm-system-packages-dpkg-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-dpkg-toggle-explicit)
    (define-key map (kbd "M-U")   'helm-system-packages-dpkg-toggle-uninstalled)
    (define-key map (kbd "M-D")   'helm-system-packages-dpkg-toggle-dependencies)
    (define-key map (kbd "M-R")   'helm-system-packages-dpkg-toggle-residuals)
    (define-key map (kbd "C-]")   'helm-system-packages-toggle-descriptions)
    map))

(defvar helm-system-packages-dpkg--show-uninstalled-p t)
(defvar helm-system-packages-dpkg--show-explicit-p t)
(defvar helm-system-packages-dpkg--show-dependencies-p t)
(defvar helm-system-packages-dpkg--show-residuals-p t)

(defun helm-system-packages-dpkg-toggle-explicit ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-dpkg--show-explicit-p
          (not helm-system-packages-dpkg--show-explicit-p))
    (helm-update)))
(put 'helm-system-packages-dpkg-toggle-explicit 'helm-only t)

(defun helm-system-packages-dpkg-toggle-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-dpkg--show-uninstalled-p
          (not helm-system-packages-dpkg--show-uninstalled-p))
    (helm-update)))
(put 'helm-system-packages-dpkg-toggle-uninstalled 'helm-only t)

(defun helm-system-packages-dpkg-toggle-dependencies ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-dpkg--show-dependencies-p
          (not helm-system-packages-dpkg--show-dependencies-p))
    (helm-update)))
(put 'helm-system-packages-dpkg-toggle-dependencies 'helm-only t)

(defun helm-system-packages-dpkg-toggle-residuals ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-dpkg--show-residuals-p
          (not helm-system-packages-dpkg--show-residuals-p))
    (helm-update)))
(put 'helm-system-packages-dpkg-toggle-residuals 'helm-only t)

(defun helm-system-packages-dpkg-transformer (packages)
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p)
                              helm-system-packages--display-lists))))
        (cond
         ((not face) (when helm-system-packages-dpkg--show-uninstalled-p
                       (push p res)))
         ((or
           (and helm-system-packages-dpkg--show-explicit-p
                (memq 'helm-system-packages-dpkg-explicit face))
           (and helm-system-packages-dpkg--show-dependencies-p
                (memq 'helm-system-packages-dpkg-dependencies face))
           (and helm-system-packages-dpkg--show-residuals-p
                (memq 'helm-system-packages-dpkg-residuals face)))
          (push (propertize p 'face (car face)) res)))))))

(defface helm-system-packages-dpkg-explicit
  '((t (:inherit font-lock-warning-face)))
  "Face for explicitly installed packages."
  :group 'helm-system-packages)

(defface helm-system-packages-dpkg-dependencies
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for packages installed as dependencies."
  :group 'helm-system-packages)

(defface helm-system-packages-dpkg-residuals
  '((t (:inherit font-lock-string-face :slant italic)))
  "Face for packages with left-over configuration files."
  :group 'helm-system-packages)

(defun helm-system-packages-dpkg-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (call-process "apt-mark" nil t nil "showmanual")
                  (buffer-string))))

(defun helm-system-packages-dpkg-list-dependencies ()
  "List packages installed as a dependency."
  (split-string (with-temp-buffer
                  (call-process "apt-mark" nil t nil "showauto")
                  (buffer-string))))

(defun helm-system-packages-dpkg-list-residuals ()
  "List packages with left-over configuration files."
  (let (res)
    (dolist (pkgline
             (split-string
              (with-temp-buffer
                (call-process "dpkg" nil t nil "--get-selections")
                (buffer-string))
              "\n")
             res)
      (let ((pkg (split-string pkgline)))
        (when (string= (cadr pkg) "deinstall")
          (push (car pkg) res))))))

(defun helm-system-packages-dpkg-cache-names () ; TODO: Build from --descriptions instead like pacman?
  "Cache all package names."
  (with-temp-buffer
    (call-process "apt-cache" nil t nil "pkgnames")
    ;; (sort-lines nil (point-min) (point-max))
    (buffer-string)))

(defcustom helm-system-packages-dpkg-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap."
  :group 'helm-system-packages
  :type 'interger)

(defun helm-system-packages-dpkg-cache-descriptions ()
  "Cache all package names with descriptions."
  (with-temp-buffer
    ;; `apt-cache search` is much faster than `apt-cache show`.
    (call-process "apt-cache" nil '(t nil) nil "search" ".")
    ;; apt-cache's output format is "pkg - desc".  Remove "-" and align to
    ;; column.
    (goto-char (point-min))
    (while (search-forward " " nil t)
      (delete-char 1)
      (backward-char)
      (let ((pos (- (point) (line-beginning-position))))
        (when (< pos helm-system-packages-dpkg-column-width)
          (insert (make-string (- helm-system-packages-dpkg-column-width pos) ? ))))
      (forward-line))
    ;; (sort-lines nil (point-min) (point-max)) ; TODO: Required? Also see helm-system-packages-dpkg-buffer-all.
    (buffer-string)))

(defun helm-system-packages-dpkg-init ()
  "Cache package lists and create Helm buffer."
  (unless (and helm-system-packages-dpkg--names helm-system-packages-dpkg--descriptions)
    (helm-system-packages-dpkg-refresh))
  (helm-init-candidates-in-buffer
      'global
    (if helm-system-packages-show-descriptions-p
        helm-system-packages-dpkg--descriptions
      helm-system-packages-dpkg--names)))

(defun helm-system-packages-dpkg-refresh ()
  "Refresh the package list."
  (interactive)
  (setq helm-system-packages-dpkg--descriptions (helm-system-packages-dpkg-cache-descriptions)
        helm-system-packages-dpkg--names (helm-system-packages-dpkg-cache-names))
  (let ((explicit (helm-system-packages-dpkg-list-explicit))
        (dependencies (helm-system-packages-dpkg-list-dependencies))
        (residuals (helm-system-packages-dpkg-list-residuals)))
    (setq helm-system-packages--display-lists nil)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-dpkg-explicit))
            helm-system-packages--display-lists))
    (dolist (p dependencies)
      (push (cons p '(helm-system-packages-dpkg-dependencies))
            helm-system-packages--display-lists))
    (dolist (p residuals)
      (push (cons p '(helm-system-packages-dpkg-residuals))
            helm-system-packages--display-lists))))

(defun helm-system-packages-dpkg-print-url (_)
  "Print homepage URLs of `helm-marked-candidates'.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (let ((res (helm-system-packages-run "apt-cache" "show"))
        urls)
    (dolist (url (split-string res "\n" t))
      (when (string-match "^Homepage: \\(.*\\)" url)
        (push (match-string 1 url) urls)))
    (helm-system-packages-browse-url urls)))

(defvar helm-system-packages-dpkg--source-name "dpkg source")

(defvar helm-system-packages-dpkg--descriptions-global nil
  "All descriptions.
Used to restore complete description list when browsing dependencies.")
(defvar helm-system-packages-dpkg--names-global nil
  "All names.
Used to restore complete name list when browsing dependencies.")

(defun helm-system-packages-dpkg-deps (_candidate &optional reverse)
  "Run a Helm session over the packages returned by COMMAND run over `helm-marked-candidates'.
With prefix argument, insert the output at point.

If REVERSE is non-nil, show reverse dependencies instead."
  (setq helm-system-packages-dpkg--descriptions
        (or helm-system-packages-dpkg--descriptions-global
            helm-system-packages-dpkg--descriptions))
  (setq helm-system-packages-dpkg--descriptions-global
        helm-system-packages-dpkg--descriptions)
  (setq helm-system-packages-dpkg--names
        (or helm-system-packages-dpkg--names-global
            helm-system-packages-dpkg--names))
  (setq helm-system-packages-dpkg--names-global
        helm-system-packages-dpkg--names)
  (let ((res (apply #'helm-system-packages-run "apt-cache"
                    (if reverse "rdepends" "depends")))
        desc-res)
    (if (string= res "")
        (message "No dependencies")
      (setq res (with-temp-buffer
                  (insert res)
                  (sort-lines nil (point-min) (point-max))
                  (delete-duplicate-lines (point-min) (point-max))
                  (buffer-string)))
      (dolist (name (split-string res "\n" t))
        (when (string-match (concat "^" name "  .*$")
                            helm-system-packages-dpkg--descriptions)
          (setq desc-res
                (concat desc-res
                        (match-string 0 helm-system-packages-dpkg--descriptions) "\n"))))
      (let ((helm-system-packages-dpkg--descriptions desc-res)
            (helm-system-packages-dpkg--names res)
            (helm-system-packages-dpkg--source-name
             (concat (if reverse "Reverse deps" "Deps")
                     " of "
                     (mapconcat 'identity (helm-marked-candidates) " "))))
        (helm-system-packages-dpkg)))))

(defcustom helm-system-packages-dpkg-actions
  '(("Show package(s)" .
     (lambda (_)
       (helm-system-packages-print "apt-cache" "show")))
    ("Install (`C-u' to reinstall)" .
     (lambda (_)
       (helm-system-packages-dpkg-run-as-root
        "apt-get" "install" (when helm-current-prefix-arg "--reinstall"))))
    ("Uninstall (`C-u' to include dependencies)" .
     (lambda (_)
       (helm-system-packages-dpkg-run-as-root
        "apt-get" "remove" (when helm-current-prefix-arg "--auto-remove"))))
    ("Browse homepage URL" . helm-system-packages-dpkg-print-url)
    ("Find files" .
     (lambda (_)
       (helm-system-packages-find-files "dpkg" "--listfiles")))
    ("Show dependencies" .
     (lambda (_)
       (helm-system-packages-print "apt-cache" "depends")))
    ("Show reverse dependencies" .
     (lambda (_)
       (helm-system-packages-print "apt-cache" "rdepends")))
    ("Uninstall/Purge (`C-u' to include dependencies)" .
     (lambda (_)
       (helm-system-packages-dpkg-run-as-root
        "apt-get" "purge" (when helm-current-prefix-arg "--auto-remove")))))
  "Actions for Helm dpkg."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-dpkg-build-source ()
  (helm-build-in-buffer-source helm-system-packages-dpkg--source-name
    :init 'helm-system-packages-dpkg-init
    :candidate-transformer 'helm-system-packages-dpkg-transformer
    :candidate-number-limit helm-system-packages-candidate-limit
    :display-to-real 'helm-system-packages-extract-name
    :keymap helm-system-packages-dpkg-map
    :help-message 'helm-system-packages-dpkg-help-message
    :persistent-help "Show package description"
    :action helm-system-packages-dpkg-actions))

(defun helm-system-packages-dpkg ()
  "Preconfigured `helm' for dpkg."
  (helm :sources (helm-system-packages-dpkg-build-source)
        :buffer "*helm dpkg*"
        :truncate-lines t
        :input (when helm-system-packages-use-symbol-at-point-p
                 (substring-no-properties (or (thing-at-point 'symbol) "")))))

(provide 'helm-system-packages-dpkg)

;;; helm-system-packages-dpkg.el ends here

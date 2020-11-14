;;; helm-system-packages-dpkg.el --- Helm UI for Debian's dpkg. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.10.2
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
(require 'helm-system-packages)


(defcustom helm-system-packages-dpkg-confirm-p t
  "By default readline interface is used to prompt questions.
If this variable is nil, then the default answers will be used
for all questions (noninteractive mode of dpkg)."
  :group 'helm-system-packages
  :type 'bool)

(defvar helm-system-packages-dpkg-help-message
  "* Helm dpkg

Requirements:

** Commands
\\<helm-system-packages-dpkg-map>
\\[helm-system-packages-dpkg-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-dpkg-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-dpkg-toggle-dependencies]\t\tToggle display of dependencies.
\\[helm-system-packages-dpkg-toggle-residuals]\t\tToggle display of package with residual configuration files.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-dpkg-map
  ;; M-U is reserved for `helm-unmark-all'.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-dpkg-toggle-explicit)
    (define-key map (kbd "M-N")   'helm-system-packages-dpkg-toggle-uninstalled)
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
                              (plist-get (helm-system-packages--cache-get) :display)))))
        (cond
         ((not face) (when helm-system-packages-dpkg--show-uninstalled-p
                       (push p res)))
         ((or
           (and helm-system-packages-dpkg--show-explicit-p
                (memq 'helm-system-packages-explicit face))
           (and helm-system-packages-dpkg--show-dependencies-p
                (memq 'helm-system-packages-dependencies face))
           (and helm-system-packages-dpkg--show-residuals-p
                (memq 'helm-system-packages-residuals face)))
          (push (propertize p 'face (car face)) res)))))))

(defun helm-system-packages-dpkg-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (process-file "apt-mark" nil t nil "showmanual")
                  (buffer-string))))

(defun helm-system-packages-dpkg-list-dependencies ()
  "List packages installed as a dependency."
  (split-string (with-temp-buffer
                  (process-file "apt-mark" nil t nil "showauto")
                  (buffer-string))))

(defun helm-system-packages-dpkg-list-residuals ()
  "List packages with left-over configuration files."
  (let (res)
    (dolist (pkgline
             (split-string
              (with-temp-buffer
                (process-file "dpkg" nil t nil "--get-selections")
                (buffer-string))
              "\n")
             res)
      (let ((pkg (split-string pkgline)))
        (when (string= (cadr pkg) "deinstall")
          (push (car pkg) res))))))

(defun helm-system-packages-dpkg-cache-names () ; TODO: Build from --descriptions instead like pacman?
  "Cache all package names."
  (with-temp-buffer
    (process-file "apt-cache" nil t nil "pkgnames")
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
    (process-file "apt-cache" nil '(t nil) nil "search" ".")
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

(defun helm-system-packages-dpkg-refresh ()
  "Refresh the package list."
  (interactive)
  (let ((explicit (helm-system-packages-dpkg-list-explicit))
        (dependencies (helm-system-packages-dpkg-list-dependencies))
        (residuals (helm-system-packages-dpkg-list-residuals))
        display-list)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-explicit))
            display-list))
    (dolist (p dependencies)
      (push (cons p '(helm-system-packages-dependencies))
            display-list))
    (dolist (p residuals)
      (push (cons p '(helm-system-packages-residuals))
            display-list))
    (helm-system-packages--cache-set
     (helm-system-packages-dpkg-cache-names)
     (helm-system-packages-dpkg-cache-descriptions)
     display-list "dpkg")))

(defun helm-system-packages-dpkg-info (candidate)
  "Print information about the selected packages.
With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (helm-system-packages-show-information
   ;; TODO: Optimize by calling the command only once and parsing output.
   `((uninstalled . ,(mapcar (lambda (pkg)
                               (cons pkg
                                     (helm-system-packages-call "apt-cache" nil "show" pkg)))
                             (if helm-in-persistent-action
                                 (list candidate)
                               (helm-marked-candidates)))))))

(defun helm-system-packages-dpkg-browse-url (_)
  "Print homepage URLs of `helm-marked-candidates'.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (let ((res (helm-system-packages-call "apt-cache" (helm-marked-candidates) "show"))
        urls)
    (dolist (url (split-string res "\n" t))
      (when (string-match "^Homepage: \\(.*\\)" url)
        (push (match-string 1 url) urls)))
    (helm-system-packages-browse-url urls)))

(defun helm-system-packages-dpkg-find-files (_)
  "Find files for marked candidates."
  (helm-system-packages-find-files
   (let ((file-hash (make-hash-table :test 'equal)))
     (dolist (pkg (helm-marked-candidates) file-hash)
       ;; TODO: Optimize by calling the command only once and parsing output.
       (dolist (file (split-string
                      (helm-system-packages-call "dpkg" nil "--listfiles" pkg)
                      "\n" t))
         (push file (gethash pkg file-hash)))))))

(defun helm-system-packages-dpkg-show-dependencies (_candidate &optional reverse)
  "List candidate dependencies for `helm-system-packages-show-packages'.
If REVERSE is non-nil, list reverse dependencies instead."
  (let ((arg (if reverse "rdepends" "depends"))
        (title (concat
                (if reverse "Reverse dependencies" "Dependencies")
                " of "
                (mapconcat 'identity (helm-marked-candidates) " "))))
    (helm-system-packages-show-packages
     `((uninstalled . ,(helm-system-packages-call
                        "apt-cache" (helm-marked-candidates) arg)))
     title)))

(defun helm-system-packages-make-apt-get-command ( &rest args)
  (let ((comm (append '("apt-get" "--quiet") args)))
    (if helm-system-packages-dpkg-confirm-p
        (push "DEBIAN_FRONTEND=readline" comm)
      (push "DEBIAN_FRONTEND=noninteractive" comm))))

(defcustom helm-system-packages-dpkg-actions
  '(("Show package(s)" . helm-system-packages-dpkg-info)
    ("Install (`C-u' to reinstall)" .
     (lambda (_)
       (apply 'helm-system-packages-run-as-root
              (helm-system-packages-make-apt-get-command "install"
                                                         (when helm-current-prefix-arg "--reinstall")))))
    ("Uninstall (`C-u' to include dependencies)" .
     (lambda (_)
       (apply 'helm-system-packages-run-as-root-over-installed
              (helm-system-packages-make-apt-get-command "remove"
                                                         (when helm-current-prefix-arg "--auto-remove")))))
    ("Browse homepage URL" . helm-system-packages-dpkg-browse-url)
    ("Find files" . helm-system-packages-dpkg-find-files)
    ("Show dependencies" . helm-system-packages-dpkg-show-dependencies)
    ("Show reverse dependencies" .
     (lambda (_)
       (helm-system-packages-dpkg-show-dependencies _ 'reverse)))
    ("Uninstall/Purge (`C-u' to include dependencies)" .
     (lambda (_)
       (apply 'helm-system-packages-run-as-root-over-installed
              (helm-system-packages-make-apt-get-command "purge"
                                                         (when helm-current-prefix-arg "--auto-remove"))))))
  "Actions for Helm dpkg."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-dpkg-build-source ()
  "Build Helm source for dpkg"
  (let ((title (or (plist-get (helm-system-packages--cache-get) :title) "package manager")))
    (helm-build-in-buffer-source title
      :init 'helm-system-packages-init
      :candidate-transformer 'helm-system-packages-dpkg-transformer
      :candidate-number-limit helm-system-packages-candidate-limit
      :display-to-real 'helm-system-packages-extract-name
      :keymap helm-system-packages-dpkg-map
      :help-message 'helm-system-packages-dpkg-help-message
      :persistent-help "Show package description"
      :action helm-system-packages-dpkg-actions)))

(defun helm-system-packages-dpkg ()
  "Preconfigured `helm' for dpkg."
  (unless (helm-system-packages-missing-dependencies-p "apt-get" "apt-cache" "apt-mark")
    (helm :sources (helm-system-packages-dpkg-build-source)
          :buffer "*helm dpkg*"
          :truncate-lines t
          :input (when helm-system-packages-use-symbol-at-point-p
                   (substring-no-properties (or (thing-at-point 'symbol) ""))))))

(provide 'helm-system-packages-dpkg)

;;; helm-system-packages-dpkg.el ends here

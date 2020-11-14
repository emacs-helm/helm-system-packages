;;; helm-system-packages-brew.el --- Helm UI for macOS homebrew. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.10.2
;; Package-Requires: ((emacs "25.1") (helm "2.8.6"))
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
;; Helm UI for macOS homebrew.

;; TODO: Add TRAMP support.
;; TODO: Implement `show-dependencies' function.
;; TODO: Show explicitly installed packages (function `helm-system-packages-brew-list-explicit' already exists).
;; TODO: Implement `find-files'.
;; TODO: Keymap.

;;; Code:
(require 'json)
(require 'helm-system-packages)

;; Shut up byte compiler
(defvar eshell-buffer-name)
(declare-function eshell-interactive-process "esh-cmd.el")
(declare-function eshell-send-input "esh-mode.el")

(defun helm-system-packages-brew-transformer (packages)
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p) helm-system-packages--display-lists))))
        (cond
         ((and (not face) helm-system-packages--show-uninstalled-p)
          (push p res)))))))

(defun helm-system-packages-brew-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (call-process "brew" nil t nil "list")
                  (buffer-string))))

(defcustom helm-system-packages-brew-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap.
If nil, then use `helm-system-packages-column-width'."
  :group 'helm-system-packages
  :type 'integer)

(defun helm-system-packages-brew-cache ()
  "Cache all package names with descriptions."
  (let (names descriptions)
    (setq descriptions
          (with-temp-buffer
            (call-process "brew" nil '(t nil) nil "desc" "-s" "")
            (buffer-string)))
    (setq names
          (replace-regexp-in-string ":.*" "" descriptions))
    (setq descriptions
          (mapconcat
           (lambda (package-from-list)
             (let* ((pkg (split-string package-from-list ": "))
                    (name (car pkg))
                    (desc (car (cdr pkg)))
                    (format-string (format "%%-%ds  %%s" helm-system-packages-column-width)))
               (format format-string name desc)))
           (split-string descriptions "\n") "\n"))
    (helm-system-packages--cache-set names descriptions nil "brew")))

(defun helm-system-packages-brew-refresh ()
  "Refresh the package list."
  (interactive)
  (setq helm-system-packages--source-name "brew source")
  (setq helm-system-packages-column-width
        (or helm-system-packages-brew-column-width
            helm-system-packages-column-width))
  (let ((res (helm-system-packages-brew-cache)))
    (setq helm-system-packages--names (car res)
          helm-system-packages--descriptions (cdr res)))
  (setq helm-system-packages--display-lists nil))

(defun helm-system-packages-brew-info (_candidate)
  "Print information about the selected packages.
With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (let* ((descriptions
          (json-read-from-string
           (with-temp-buffer
             (apply 'call-process "brew" nil t nil "info" "--json=v1" (helm-marked-candidates))
             (buffer-string))))
         desc-list
         pkg-desc-alist
         str
         (i 0))
    (dolist (pkg (helm-marked-candidates))
      (setq pkg-desc-alist (aref descriptions i))
      (setq str
            (concat
             "* Description: " (alist-get 'desc pkg-desc-alist) "\n"
             "* Version: " (alist-get 'stable (alist-get 'versions pkg-desc-alist)) "\n"
             "* URL: " (alist-get 'homepage pkg-desc-alist) "\n" "\n"
             "* Dependencies:\n" "   " (mapconcat 'identity (alist-get 'dependencies pkg-desc-alist) "\n   ") "\n\n"
             "* Optional dependencies:\n" "   " (mapconcat 'identity (alist-get 'optional_dependencies pkg-desc-alist) "\n   ") "\n\n"
             "* Options:\n" (mapconcat (lambda (pkg-option)
                                         (concat (alist-get 'option pkg-option) "\n"
                                                 "    " (alist-get 'description pkg-option) "\n"))
                                       (alist-get 'options pkg-desc-alist) "\n")
             "\n\n"
             "* Caveats: " (alist-get 'caveats pkg-desc-alist) "\n"))
      (push `(uninstalled (,pkg . ,str)) desc-list)
      (setq i (1+ i)))
    (helm-system-packages-show-information desc-list)))

(defun helm-system-packages-brew-browse-url (_candidate)
  (let ((descriptions
         (json-read-from-string
          (with-temp-buffer
            (apply 'call-process "brew" nil t nil "info" "--json=v1" (helm-marked-candidates))
            (buffer-string)))))
    (helm-system-packages-browse-url
     (mapcar (lambda (pkg)
               (alist-get 'homepage pkg))
             descriptions))))

(defun helm-system-packages-brew-link-app (_candidate)
  (helm-system-packages-brew-run  "brew" "link"))

(defun helm-system-packages-brew-unlink-app (_candidate)
  (helm-system-packages-brew-run  "brew" "unlink"))

(defun helm-system-packages-brew-run (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.
COMMAND will be run in an Eshell buffer `helm-system-packages-eshell-buffer'.
COMMAND is run without sudo as macOS brew does not require sudo rights."
  (require 'esh-mode)
  (let ((arg-list (append args (helm-marked-candidates)))
        (eshell-buffer-name helm-system-packages-shell-buffer-name))
    ;; Refresh package list after command has completed.
    (push command arg-list)
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

(defcustom helm-system-packages-brew-actions
  '(("Show package(s)" . helm-system-packages-brew-info)
    ("Install (`C-u' to reinstall)" .
     (lambda (_)
       (if helm-current-prefix-arg
           (helm-system-packages-brew-run  "brew" "reinstall")
         (helm-system-packages-brew-run "brew" "install"))))
    ("Uninstall (`C-u' to uninstall all versions)" .
     (lambda (_)
       (helm-system-packages-brew-run "brew" "uninstall"
                                      (when helm-current-prefix-arg "--force"))))
    ("Browse homepage URL" . helm-system-packages-brew-browse-url)
    ("Link application" . helm-system-packages-brew-link-app)
    ("Unlink application" . helm-system-packages-brew-unlink-app))
  "Actions for Helm brew."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-brew-build-source ()
  "Build Helm source for brew."
  (let ((title (or (plist-get (helm-system-packages--cache-get) :title) "package manager")))
    (helm-build-in-buffer-source title
      :init 'helm-system-packages-init
      :candidate-transformer 'helm-system-packages-brew-transformer
      :candidate-number-limit helm-system-packages-candidate-limit
      :display-to-real 'helm-system-packages-extract-name
      :persistent-help "Show package description"
      :action helm-system-packages-brew-actions)))

(defun helm-system-packages-brew ()
  "Preconfigured `helm' for brew."
  (helm :sources (helm-system-packages-brew-build-source)
        :buffer "*helm brew*"
        :truncate-lines t
        :input (when helm-system-packages-use-symbol-at-point-p
                 (substring-no-properties (or (thing-at-point 'symbol) "")))))

(provide 'helm-system-packages-brew)

;;; helm-system-packages-brew.el ends here

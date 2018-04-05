;;; helm-system-packages-brew.el --- Helm UI for macOS homebrew. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
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
;; Helm UI for macOS homebrew.

;; TODO
;; (1) Implement `show-dependencies' function
;; (2) Show packages explicitly installed (function `helm-system-packages-brew-list-explicit' already exists)
;; (3) Implement `find-files'
;; (4) Keymap

;;; Code:
(require 'helm)
(require 'json)
(require 'helm-system-packages)

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
If nil, then use `helm-system-package-column-width'."
  :group 'helm-system-packages
  :type 'integer)

(defun helm-system-packages-brew-cache ()
  "Cache all package names with descriptions.
LOCAL-PACKAGES and GROUPS are lists of strings.
Return (NAMES . DESCRIPTIONS), a cons of two strings."
  ;; We build both caches at the same time.  We could also build just-in-time, but
  ;; benchmarks show that it only saves less than 20% when building one cache.
  (let (names descriptions)
    (setq descriptions
          (with-temp-buffer
            ;; TODO: Possible optimization: Output directly in Elisp?
            (let ((format-string (format "%%-%dn  %%d" helm-system-packages-column-width)))
              (call-process "brew" nil '(t nil) nil "desc" "-s" "" )
	      (buffer-string))))
    (setq names
	  (replace-regexp-in-string ":.*" "" descriptions))      
    (setq descriptions (mapconcat (lambda (package-from-list)
				    (let* ((pkg (split-string package-from-list ": "))
					   (name (car pkg))
					   (desc (car (cdr pkg)))
					   (format-string (format "%%-%ds  %%s" helm-system-packages-column-width)))
				    (format format-string name desc)))
				  (split-string descriptions"\n") "\n" ))
  (cons names descriptions)))

(defun helm-system-packages-brew-init ()
  "Cache package lists and create Helm buffer."
  (unless (and helm-system-packages--names helm-system-packages--descriptions)
    (helm-system-packages-brew-refresh))
  ;; TODO: We should only create the buffer if it does not already exist.
  ;; On the other hand, we need to be able to override the package list.
  ;; (unless (helm-candidate-buffer) ...
  (helm-init-candidates-in-buffer
      'global
    (if helm-system-packages-show-descriptions-p
        helm-system-packages--descriptions
      helm-system-packages--names)))

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
  (let* ((descriptions (json-read-from-string (with-temp-buffer
						(apply 'call-process "brew" nil t nil "info" "--json=v1" (helm-marked-candidates)) 
						(buffer-string))))
	 desc-list
	 pkg-desc-alist
	 str
	 pkg
	 (i 0))
    (dolist (pkg (helm-marked-candidates))
      (setq pkg-desc-alist (aref descriptions i))
      (setq str (concat "* Description: " (alist-get 'desc pkg-desc-alist) "\n"
	      "* Version: "(alist-get 'stable (alist-get 'versions pkg-desc-alist)) "\n"
	      "* URL: "(alist-get 'homepage pkg-desc-alist) "\n"  "\n"
	      "* Dependencies:\n" "   " (mapconcat 'identity (alist-get 'dependencies pkg-desc-alist) "\n   ") "\n\n"
	      "* Optional dependencies:\n" "   "(mapconcat 'identity (alist-get 'optional_dependencies pkg-desc-alist) "\n   ") "\n\n"
	      "* Options:\n" (mapconcat (lambda (pkg-option)
					(concat (alist-get 'option pkg-option)  "\n"
						"    " (alist-get 'description pkg-option)  "\n"))
				      (alist-get 'options pkg-desc-alist) "\n")
	      "\n\n"
	      "* Caveats: " (alist-get 'caveats pkg-desc-alist) "\n"))
     (add-to-list 'desc-list `(uninstalled (,pkg . ,str)))
     (setq i (1+ i)))
    (helm-system-packages-show-information desc-list)))

(defun helm-system-package-brew-browse-url (_candidate)
   (let* ((descriptions (json-read-from-string (with-temp-buffer
						(apply 'call-process "brew" nil t nil "info" "--json=v1" (helm-marked-candidates)) 
						(buffer-string)))))
    (helm-system-packages-browse-url (mapcar (lambda (pkg)
					       (alist-get 'homepage pkg))
					     descriptions))))

(defun helm-system-package-brew-link-app (_candidate)
  (helm-system-packages-brew-run  "brew" "link"))

(defun helm-system-package-brew-unlink-app (_candidate)
  (helm-system-packages-brew-run  "brew" "unlink"))

(defun helm-system-packages-brew-run (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

COMMAND will be run in an Eshell buffer `helm-system-packages-eshell-buffer'."
  (require 'esh-mode)
  (let ((arg-list (append args (helm-marked-candidates)))
        (eshell-buffer-name helm-system-packages-eshell-buffer))
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
    ("Browse homepage URL" . helm-system-package-brew-browse-url)
    ("Link application" . helm-system-package-brew-link-app)
    ("Unlink application" . helm-system-package-brew-unlink-app))
  "Actions for Helm brew."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-brew-build-source ()
  "Build Helm source for brew."
  (helm-build-in-buffer-source helm-system-packages--source-name
    :init 'helm-system-packages-brew-init
    :candidate-transformer 'helm-system-packages-brew-transformer
    :candidate-number-limit helm-system-packages-candidate-limit
    :display-to-real 'helm-system-packages-extract-name
    :persistent-help "Show package description"
    :action helm-system-packages-brew-actions))

(defun helm-system-packages-brew ()
  "Preconfigured `helm' for brew."
    (helm :sources (helm-system-packages-brew-build-source)
          :buffer "*helm brew*"
          :truncate-lines t
          :input (when helm-system-packages-use-symbol-at-point-p
                   (substring-no-properties (or (thing-at-point 'symbol) "")))))

(provide 'helm-system-packages-brew)

;;; helm-system-packages-brew.el ends here

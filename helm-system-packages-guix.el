;;; helm-system-packages-guix.el --- Helm UI for Guix. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018, 2020 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.10.2
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
(require 'helm-system-packages)

;; Shut up byte compiler
(defvar eshell-buffer-name)
(declare-function eshell-interactive-process "esh-cmd.el")
(declare-function eshell-send-input "esh-mode.el")

;; TODO: Add support for superseded and obsolete packages.
;; TODO: Add support for multiple outputs (install, uninstall, listing...).
;; TODO: Add support for multiple versions.

(defvar helm-system-packages-guix-dependencies '("guix")
  "Dependencies needed by Guix.")

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

(defun helm-system-packages-guix-el->scheme-syntax (form)
  ;; Escape symbols are printed as \\NAME while they should be printed as NAME.
  (replace-regexp-in-string "\\\\" "" (format "%S" form)))

(cl-defun helm-system-packages-guix-eval (form &rest more-forms)
  "Evaluate forms in Guix REPL.
Return the REPL output (including the error output) as a string."
  (let ((temp-file))
    (unwind-protect
        (let ((inhibit-message t))
          ;; Inhibit file saving message of temp-file since it's an
          ;; implementation detail.
          (setq temp-file (make-temp-file "helm-system-packages-guix"))
          (with-temp-buffer
            (dolist (f (cons form more-forms))
              (insert (helm-system-packages-guix-el->scheme-syntax f)))
            (write-region (point-min) (point-max) temp-file))
          (with-output-to-string
            (with-current-buffer standard-output
              (process-file "guix" nil '(t t) nil "repl" temp-file))))
      (delete-file temp-file))))

(defvar helm-system-packages-guix-cache-file
  (expand-file-name "helm-system-packages-guix" user-emacs-directory)
  "Filename of the cache storing all Guix package descriptions.")
(make-obsolete-variable 'helm-system-packages-guix-cache-file nil "1.10.2")

(defvar helm-system-packages-guix-path
  (expand-file-name "current" "~/.config/guix")
  "Path to the latest guix checkout.")
(make-obsolete-variable 'helm-system-packages-guix-path nil "1.10.2")

(defun helm-system-packages-generate-database ()
  (helm-system-packages-guix-eval
   '(use-modules
     (guix packages)
     (guix licenses)
     (guix utils)
     (guix build utils)                 ; For `string-replace-substring'.
     (gnu packages))

   '(define (ensure-list l)
     (if (list? l)
         l
         (list l)))

   '(format '\#t "(~&")
   '(fold-packages
     (lambda (package count)
       (let ((location (package-location package)))
         (format '\#t "(~s (:version ~s :outputs ~s :supported-systems ~s :inputs ~s :propagated-inputs ~s :native-inputs ~s :location ~s :home-page ~s :licenses ~s :synopsis ~s :description ~s))~&"
                 (package-name package)
                 (package-version package)
                 (package-outputs package)
                 (package-supported-systems package)
                 (map car (package-inputs package))
                 (map car (package-propagated-inputs package))
                 (map car (package-native-inputs package))
                 (string-join (list (location-file location)
                                    (number->string (location-line location))
                                    (number->string (location-column location)))
                              ":")
                 (or (package-home-page package) "") ; #f must be turned to NIL for Emacs Lisp.
                 (map license-name (ensure-list (package-license package)))
                 (package-synopsis package)
                 (package-description package)))
       (+ 1 count))
     1)
   '(format '\#t "~&)~&")))

(defvar helm-system-packages-guix--databases (make-hash-table :test 'equal)
  "Hash-table databases indexed by host.
See `helm-system-packages-guix-database-index'.")

(defun helm-system-packages-guix-database-index ()
  "Return hostname corresponding to `default-directory'"
  (if (tramp-tramp-file-p default-directory)
      (tramp-file-name-host
       (tramp-dissect-file-name default-directory))
    "localhost"))

(defun helm-system-packages-guix-get-database ()
  (let* ((index (helm-system-packages-guix-database-index)))
    (or (gethash index helm-system-packages-guix--databases)
        (progn
          (message "Building package database...")
          (let ((result (cl-sort
                         (read (helm-system-packages-generate-database))
                         #'string< :key #'car)))
            (puthash index result helm-system-packages-guix--databases)
            result)))))

(defun helm-system-packages-guix-cache (display-list)
  "Cache all package names with descriptions. "
  ;; We build both caches at the same time.  We could also build just-in-time, but
  ;; benchmarks show that it only saves less than 20% when building one cache.
  (let* (names
         descriptions)
    (setq descriptions
          (mapconcat (lambda (name+props)
                       (let ((name (car name+props))
                             (synopsis (plist-get (cadr name+props) :synopsis)))
                         (format "%s%s%s"
                                 name
                                 (make-string (max (- helm-system-packages-column-width
                                                      (length name))
                                                   1)
                                              ? )
                                 synopsis)))
                     (helm-system-packages-guix-get-database)
                     "\n"))
    ;; replace-regexp-in-string is faster than mapconcat over split-string.
    (setq names
          (replace-regexp-in-string " .*" "" descriptions))
    (helm-system-packages--cache-set names descriptions display-list "guix")))

(defun helm-system-packages-guix-refresh ()
  "Refresh the list of installed packages."
  (interactive)
  (remhash (helm-system-packages-guix-database-index) helm-system-packages-guix--databases)
  (let* ((explicit (helm-system-packages-guix-list-explicit))
         display-list)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-explicit)) display-list))
    (helm-system-packages-guix-cache display-list)))

(defun helm-system-packages-guix-info (candidate)
  "Print information about the selected packages.
With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (cl-flet ((format-property
             (prop)
             (cond
              ((listp prop)
               (string-join prop " "))
              (nil "")
              (t prop))))
    (helm-system-packages-show-information
     `((uninstalled .
                    ,(mapcar
                      (lambda (name)
                        (let ((props (car (alist-get
                                           name
                                           (helm-system-packages-guix-get-database)
                                           nil nil #'string=))))
                          (cons name
                                (string-join
                                 (cl-loop for (key property) on props by #'cddr
                                          collect (format "%s: %s"
                                                          (substring (prin1-to-string key) 1)
                                                          (format-property property)))
                                 "\n"))))
                      (if helm-in-persistent-action
                          (list candidate)
                        (helm-marked-candidates))))))))

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
        (add-hook 'eshell-post-command-hook 'helm-system-packages-guix-refresh nil t)
        (add-hook 'eshell-post-command-hook
                  (lambda () (remove-hook 'eshell-post-command-hook 'helm-system-packages-guix-refresh t))
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
   (mapcar (lambda (name) (plist-get
                           (car (alist-get name (helm-system-packages-guix-get-database)
                                           nil nil #'string=))
                           :home-page))
           (helm-marked-candidates))))

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

(defun helm-system-packages-guix-show-dependencies (_candidate)
  "List candidate dependencies for `helm-system-packages-show-packages'. "
  (let ((title (concat
                "Dependencies of "
                (mapconcat 'identity (helm-marked-candidates) " "))))
    (helm-system-packages-show-packages
     `((uninstalled . ,(string-join
                        (mapcan
                         (lambda (name)
                           (let ((props (car (alist-get
                                              name
                                              (helm-system-packages-guix-get-database)
                                              nil nil #'string=))))
                             (append (plist-get props :inputs)
                                     (plist-get props :propagated-inputs))))
                         (helm-marked-candidates))
                        "\n")))
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

(defvar helm-system-packages-guix
  (helm-system-packages-manager-create
   :name "guix"
   :refresh-function #'helm-system-packages-guix-refresh
   :dependencies helm-system-packages-guix-dependencies
   :help-message 'helm-system-packages-guix-help-message
   :keymap helm-system-packages-guix-map
   :transformer #'helm-system-packages-guix-transformer
   :actions helm-system-packages-guix-actions))

(provide 'helm-system-packages-guix)

;;; helm-system-packages-guix.el ends here

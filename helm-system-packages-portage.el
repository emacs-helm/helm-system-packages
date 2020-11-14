;;; helm-system-packages-portage.el --- Helm UI for Portage. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
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
;; Helm UI for Portage.

;;; Code:
(require 'helm-system-packages)

(defvar helm-system-packages-portage-help-message
  "* Helm Portage

** Commands
\\<helm-system-packages-portage-map>
\\[helm-system-packages-portage-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-portage-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-portage-toggle-dependencies]\t\tToggle display of dependencies.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-portage--show-uninstalled-p t)
(defvar helm-system-packages-portage--show-explicit-p t)
(defvar helm-system-packages-portage--show-dependencies-p t)

(defvar helm-system-packages-portage-map
  ;; M-U is reserved for `helm-unmark-all'.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-toggle-explicit)
    (define-key map (kbd "M-N")   'helm-system-packages-toggle-uninstalled)
    (define-key map (kbd "M-D")   'helm-system-packages-toggle-dependencies)
    (define-key map (kbd "C-]")   'helm-system-packages-toggle-descriptions)
    map))

(defun helm-system-packages-portage-transformer (packages)
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p)
                              (plist-get (helm-system-packages--cache-get) :display)))))
        (cond
         ((and (not face) helm-system-packages--show-uninstalled-p)
          (push p res))
         ((or
           (and helm-system-packages--show-explicit-p (memq 'helm-system-packages-explicit face))
           (and helm-system-packages--show-dependencies-p (memq 'helm-system-packages-dependencies face)))
          (push (propertize p 'face (car face)) res)))))))

(defun helm-system-packages-portage-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (insert-file-contents-literally "/var/lib/portage/world")
                  (buffer-string))))

(defun helm-system-packages-portage-list-dependencies (&optional explicit)
  "List packages installed as a required dependency.
The caller can pass the list of EXPLICIT packages to avoid re-computing it."
  (unless explicit
    (setq explicit (helm-system-packages-portage-list-explicit)))
  (seq-difference
   (split-string (with-temp-buffer
                   (process-file "qlist" nil t nil "-I")
                   (buffer-string)))
   explicit))

(defun helm-system-packages-portage-cache (display-list)
  "Cache all package names with descriptions."
  (let* ((raw (with-temp-buffer
                ;; TODO: Can eix pad in the format string just like `expac' does?
                ;; TODO: Or output straight to Elisp?
                (process-file "env" nil '(t nil) nil
                              "EIX_LIMIT=0" "OVERLAYS_LIST=none" "PRINT_COUNT_ALWAYS=never"
                              "eix" "--format" "<category>/<name>\t<description>\n")
                (sort-lines nil (point-min) (point-max))
                (buffer-string)))
         (paired (mapcar (lambda (x) (let ((l (split-string x "\t"))) (cons (car l) (cadr l))))
                         (split-string raw "\n")))
         (names (mapcar 'car paired))
         (descriptions (mapcar (lambda (x)
                                 (concat (car x)
                                         (make-string
                                          (max (- helm-system-packages-column-width
                                                  (length (car x)))
                                               0)
                                          ? )
                                         (cdr x)))
                               paired)))
    (helm-system-packages--cache-set names descriptions display-list "portage")))

(defcustom helm-system-packages-portage-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap.
If nil, then use `helm-system-packages-column-width'."
  :group 'helm-system-packages
  :type 'integer)

(defun helm-system-packages-portage-refresh ()
  "Refresh the package list."
  (interactive)
  (setq helm-system-packages-column-width
        (or helm-system-packages-portage-column-width
            helm-system-packages-column-width))
  (let ((explicit (helm-system-packages-portage-list-explicit))
        (dependencies (helm-system-packages-portage-list-dependencies))
        display-list)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-explicit)) display-list))
    (dolist (p dependencies)
      (push (cons p '(helm-system-packages-dependencies)) display-list))
    (helm-system-packages-portage-cache display-list)))

(defcustom helm-system-packages-portage-actions
  '(("Show package(s)" .
     (lambda (_)
       (helm-system-packages-print "eix")))
    ("Install (`C-u' to not add to world)" .
     (lambda (_)
       (helm-system-packages-run-as-root "emerge" "--ask" "--verbose" (when helm-current-prefix-arg "--oneshot"))))
    ("Uninstall (`C-u' to include dependencies)" .
     (lambda (_)
       (helm-system-packages-run-as-root-over-installed "emerge" "--ask" "--verbose" (if helm-current-prefix-arg "--depclean" "--unmerge"))))
    ("Browse homepage URL" .
     (lambda (_)
       (helm-system-packages-browse-url (split-string (helm-system-packages-run "eix" "--format" "<homepage>\n") "\n" t))))
    ("Find files" .
     (lambda (_)
       (helm-system-packages-print "equery" "--no-color" "files")))
    ("Show dependencies" .
     (lambda (_)
       (helm-system-packages-print "equery" "--no-color" "depgraph")))
    ("Show reverse dependencies" .
     (lambda (_)
       (helm-system-packages-print "equery" "--no-color" "depends")))
    ("Emerge-pretend" .
     (lambda (_)
       (helm-system-packages-print "emerge" "--pretend")))
    ("Show history" .
     (lambda (_)
       (helm-system-packages-print "genlop" "-qe")))
    ("Show extra info" .
     (lambda (_)
       (helm-system-packages-print "genlop" "-qi")))
    ("Show USE flags" .
     (lambda (_)
       (helm-system-packages-print "equery" "--no-color" "uses")
       ;; TODO: Test font-lock.
       (unless helm-current-prefix-arg
         (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
         (font-lock-mode 1)))))
  "Actions for Helm portage."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-portage-build-source ()
  "Build Helm source for portage."
  (let ((title (or (plist-get (helm-system-packages--cache-get) :title) "package manager")))
    (helm-build-in-buffer-source title
      :init 'helm-system-packages-init
      :candidate-transformer 'helm-system-packages-portage-transformer
      :candidate-number-limit helm-system-packages-candidate-limit
      :display-to-real 'helm-system-packages-extract-name
      :keymap helm-system-packages-portage-map
      :help-message 'helm-system-packages-portage-help-message
      :persistent-help "Show package description"
      :action helm-system-packages-portage-actions)))

(defun helm-system-packages-portage-use-init ()
  "Initialize buffer with all USE flags."
  (unless (helm-candidate-buffer)
    (let ((local-uses (split-string (with-temp-buffer
                                      (process-file "portageq" nil t nil "envvar" "USE")
                                      (buffer-string)))))
      (helm-init-candidates-in-buffer
          'global
        (mapcar (lambda (use-flag) (propertize use-flag 'face (when (member use-flag local-uses) 'helm-system-packages-explicit)))
                (split-string (with-temp-buffer
                                (process-file "eix" nil t nil "--print-all-useflags")
                                (buffer-string))))))))

(defcustom helm-system-packages-portage-use-actions
  '(("Description" .
     (lambda (elm)
       (switch-to-buffer helm-system-packages-buffer)
       (erase-buffer)
       (apply #'process-file "euse" nil t nil `("--info" ,elm))
       (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
       (font-lock-mode 1)))
    ("Enable" .
     (lambda (_)
       (helm-system-packages-run-as-root "euse" "--enable")))
    ("Disable" .
     (lambda (_)
       (helm-system-packages-run-as-root "euse" "--disable")))
    ("Remove" .
     (lambda (_)
       (helm-system-packages-run-as-root "euse" "--prune")))
    ("Show which dependencies use this flag" .
     (lambda (_)
       (helm-system-packages-print "equery" "--no-color" "hasuse"))))
  "Actions for Helm portage USE flags."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defvar helm-system-packages-portage-use-source
  (helm-build-in-buffer-source "USE flags"
    :init 'helm-system-packages-portage-use-init
    :help-message 'helm-system-packages-portage-help-message
    :action helm-system-packages-portage-use-actions))

(defun helm-system-packages-portage ()
  "Preconfigured `helm' for Portage."
  (unless (helm-system-packages-missing-dependencies-p "eix" "qlist" "euse" "portageq" "genlop")
    (helm :sources (list (helm-system-packages-portage-build-source)
                         helm-system-packages-portage-use-source)
          :buffer "*helm portage*"
          :truncate-lines t
          :input (when helm-system-packages-use-symbol-at-point-p
                   (substring-no-properties (or (thing-at-point 'symbol) ""))))))

(provide 'helm-system-packages-portage)

;;; helm-system-packages-portage.el ends here

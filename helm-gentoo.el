;;; helm-gentoo.el --- Helm UI for gentoo portage. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017        Pierre Neidhardt <ambrevar@gmail.com>

;; Version: 1.6.9
;; Package-Requires: ((helm "2.8.6"))

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
;; Helm UI for portage.

;;; Code:
(require 'cl-lib)
(require 'helm)

(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-send-eof "term")

;; TODO: Rename "Gentoo" to "Portage".
;; TODO: Namespace everything.


(defgroup helm-gentoo nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defface helm-gentoo-local '((t (:foreground "orange")))
  "Face for installed packages or local USE flags in `helm-gentoo'."
  :group 'traverse-faces)


;;; Internals
(defvar helm-gentoo-buffer "*helm-gentoo-output*")
(defvar helm-cache-gentoo nil)
(defvar helm-cache-world nil)

(defun helm-gentoo-init ()
  (unless (helm-candidate-buffer)
    (helm-init-candidates-in-buffer
        'global
      (with-temp-buffer
        (unless helm-cache-gentoo
          (helm-gentoo-setup-cache))
        (unless helm-cache-world
          (setq helm-cache-world (helm-gentoo-get-world)))
        (dolist (i helm-cache-gentoo)
          (insert (concat i "\n")))
        (buffer-string)))))

(defvar helm-source-gentoo
  (helm-build-in-buffer-source "Portage source"
    :init 'helm-gentoo-init
    :candidate-transformer 'helm-highlight-world
    :action  '(("Show package" . (lambda (elm)
                                   (helm-gentoo-eshell-action elm "eix")))
               ("Show history" . (lambda (elm)
                                   (if (member elm helm-cache-world)
                                       (helm-gentoo-eshell-action elm "genlop -qe")
                                     (message "No info on non-installed packages"))))
               ("Copy in kill-ring" . kill-new)
               ("Insert at point" . insert)
               ("Browse homepage URL (`C-u' inserts at point)" . (lambda (elm)
                                                                   (let ((urls (helm-gentoo-get-url elm)))
                                                                     (if helm-current-prefix-arg
                                                                         (insert urls)
                                                                       (browse-url (helm-comp-read "URL: " urls :must-match t))))))
               ("Show extra info" . (lambda (elm)
                                      (if (member elm helm-cache-world)
                                          (helm-gentoo-eshell-action elm "genlop -qi")
                                        (message "No info on non-installed packages"))))
               ("Show USE flags (`C-u' inserts at point)" . (lambda (elm)
                                                              (helm-gentoo-default-action elm "equery" "-C" "u")
                                                              (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                                                              (font-lock-mode 1)))
               ("Emerge-pretend" . (lambda (elm)
                                     (helm-gentoo-eshell-action elm "emerge -p")))
               ("Emerge" . (lambda (elm)
                             (helm-gentoo-install elm :action 'install)))
               ("Unmerge" . (lambda (elm)
                              (helm-gentoo-install elm :action 'uninstall)))
               ("Show dependencies (`C-u' inserts at point)" . (lambda (elm)
                                                                 (helm-gentoo-default-action elm "equery" "-C" "d")))
               ("Show related files (`C-u' inserts at point)" . (lambda (elm)
                                                                  ;; TODO: Use helm-read-file or similar?
                                                                  (helm-gentoo-default-action elm "equery" "-C" "files")))
               ("Refresh" . (lambda (elm)
                              (helm-gentoo-setup-cache)
                              (setq helm-cache-world (helm-gentoo-get-world)))))))

;; TODO: Test if cache gets updated on install/uninstall.
;; TODO: Optionally run in Eshell?
(cl-defun helm-gentoo-install (_candidate &key action)
  (setq helm-external-commands-list nil)
  (ansi-term (getenv "SHELL") "Gentoo emerge")
  (term-line-mode)
  (let ((command (cl-case action
                   (install "sudo emerge -av ")
                   (uninstall "sudo emerge -avC ")
                   (t (error "Unknown action"))))
        (elms (mapconcat 'identity (helm-marked-candidates) " ")))
    (goto-char (point-max))
    (insert (concat command elms))
    (term-char-mode) (term-send-input)))

(defun helm-gentoo-default-action (elm command &rest args)
  "Gentoo default action that use `helm-gentoo-buffer'."
  (if (member elm helm-cache-world)
      (let* ((com-list (append args (list elm)))
             (res (with-temp-buffer
                    (apply #'call-process command nil t nil com-list)
                    (buffer-string))))
        (if (string-empty-p res)
            (message "No result")
          (unless helm-current-prefix-arg
              (switch-to-buffer helm-gentoo-buffer)
              (erase-buffer))
          (insert res)))
  (message "No info on non-installed packages")))

(defvar helm-source-use-flags
  (helm-build-in-buffer-source "USE flags"
    :init 'helm-gentoo-use-init
    :candidate-transformer 'helm-highlight-local-use
    :action '(("Description"
               . (lambda (elm)
                   (switch-to-buffer helm-gentoo-buffer)
                   (erase-buffer)
                   (apply #'call-process "euse" nil t nil
                          `("-i"
                            ,elm))
                   (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                   (font-lock-mode 1)))
              ("Enable"
               . (lambda (elm)
                   (helm-gentoo-eshell-action elm "*sudo -p Password: euse -E")))
              ("Disable"
               . (lambda (elm)
                   (helm-gentoo-eshell-action elm "*sudo -p Password: euse -D")))
              ("Remove"
               . (lambda (elm)
                   (helm-gentoo-eshell-action elm "*sudo -p Password: euse -P")))
              ("Show which deps use this flag"
               . (lambda (elm)
                   (switch-to-buffer helm-gentoo-buffer)
                   (erase-buffer)
                   (apply #'call-process "equery" nil t nil
                          `("-C"
                            "h"
                            ,elm)))))))

(defun helm-gentoo-setup-cache ()
  "Set up `helm-cache-gentoo'"
  (setq helm-cache-gentoo
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--only-names")
                        (buffer-string)))))

(defun helm-gentoo-eshell-action (elm command)
  (when (get-buffer "*Eshell Command Output*")
    (kill-buffer "*Eshell Command Output*"))
  (message "Searching...")
  (let ((buf-fname (buffer-file-name helm-current-buffer)))
    (if (and buf-fname (string-match tramp-file-name-regexp buf-fname))
        (progn
          (save-window-excursion ; TODO: Fix this brittle mechanism.
            (pop-to-buffer "*scratch*")
            (eshell-command (format "%s %s" command elm)))
          (pop-to-buffer "*Eshell Command Output*"))
      (eshell-command (format "%s %s" command elm)))))

(defun helm-gentoo-use-init ()
  "Initialize buffer with all USE flags."
  (unless (helm-candidate-buffer)
    (helm-init-candidates-in-buffer
        'global
      (with-temp-buffer
        (call-process "eix" nil t nil
                      "--print-all-useflags")
        (buffer-string)))))

(defun helm-gentoo-get-url (elm)
  "Return a list of URLs from eix output."
  (split-string
   (with-temp-buffer
     (call-process "eix" nil t nil elm "--format" "<homepage>\n")
     (buffer-string))))

(defun helm-gentoo-get-world ()
  "Return list of all installed package on your system."
  (split-string (with-temp-buffer
                  (call-process "qlist" nil t nil
                                "-I")
                  (buffer-string))))

(defun helm-gentoo-get-local-use ()
  (split-string (with-temp-buffer
                  (call-process "portageq" nil t nil
                                "envvar"
                                "USE")
                  (buffer-string))))

(defun helm-highlight-world (eix)
  "Highlight all installed package."
  (cl-loop for i in eix
        if (member i helm-cache-world)
        collect (propertize i 'face 'helm-gentoo-local)
        else
        collect i))

(defun helm-highlight-local-use (use-flags)
  (let ((local-uses (helm-gentoo-get-local-use)))
    (cl-loop for i in use-flags
          if (member i local-uses)
          collect (propertize i 'face 'helm-gentoo-local)
          else
          collect i)))

(defun helm-gentoo ()
  "Preconfigured `helm' for Portage."
  (helm-other-buffer '(helm-source-gentoo
                       helm-source-use-flags)
                     "*helm portage*"))

(provide 'helm-gentoo)

;; TODO: Remove local variables?
;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gentoo.el ends here

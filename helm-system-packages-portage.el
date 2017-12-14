;;; helm-system-packages-portage.el --- Helm UI for Portage. -*- lexical-binding: t -*-

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
;; Helm UI for Portage.

;;; Code:
(require 'helm)
(require 'helm-system-packages)

(defgroup helm-system-packages-portage nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defun helm-system-packages-portage-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (insert-file-contents-literally "/var/lib/portage/world")
                  (buffer-string))))

(defun helm-system-packages-portage-list-dependencies ()
  "List packages installed as a dependency."
  (unless helm-system-packages--explicit
    (helm-system-packages-portage-list-explicit))
  (seq-difference
   (split-string (with-temp-buffer
                   (call-process "qlist" nil t nil "-I")
                   (buffer-string)))
   helm-system-packages--explicit))

(defun helm-system-packages-portage-list-all ()
  "List all packages."
  (split-string (with-temp-buffer
                  (call-process "eix" nil t nil "--only-names")
                  (buffer-string))))

(defun helm-system-packages-portage-print-url (_)
  "Print homepage URLs of `helm-marked-candidates'.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (let ((urls (helm-system-packages-run "eix" "--format" "<homepage>\n")))
    (if (string-empty-p urls)
        (message "No result")
      (if helm-current-prefix-arg
          (insert urls)
        (browse-url (helm-comp-read "URL: " (split-string urls) :must-match t))))))

(defvar helm-system-packages-portage-source
  (helm-build-in-buffer-source "Portage source"
    :init 'helm-system-packages-init
    :candidate-transformer 'helm-system-packages-highlight
    :action '(("Show package(s)" .
               (lambda (_)
                 (helm-system-packages-print "eix")))
              ("Install" .
               (lambda (_)
                 (helm-system-packages-run-as-root "emerge" "--ask" "--verbose")))
              ("Uninstall" .
               (lambda (_)
                 (helm-system-packages-run-as-root "emerge" "--ask" "--verbose" "-C")))
              ("Emerge-pretend" .
               (lambda (_)
                 (helm-system-packages-print "emerge" "-p")))
              ("Find files" .
               (lambda (_)
                 (helm-system-packages-print "equery" "-C" "files")))
              ("Show dependencies" .
               (lambda (_)
                 (helm-system-packages-print "equery" "-C" "g")))
              ("Show reverse dependencies" .
               (lambda (_)
                 (helm-system-packages-print "equery" "-C" "d")))
              ("Show history" .
               (lambda (_)
                 (helm-system-packages-print "genlop" "-qe")))
              ("Show extra info" .
               (lambda (_)
                 (helm-system-packages-print elm "genlop -qi")))
              ("Show USE flags" .
               (lambda (_)
                 (helm-system-packages-print elm "equery" "-C" "u")
                 ;; TODO: Test font-lock.
                 (unless helm-current-prefix-arg
                   (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                   (font-lock-mode 1))))
              ("Browse homepage URL" . helm-system-packages-portage-print-url)
              ("Refresh" . 'helm-system-packages-refresh))))

(defun helm-system-packages-portage-use-init ()
  "Initialize buffer with all USE flags."
  (unless (helm-candidate-buffer)
    (helm-init-candidates-in-buffer
        'global
      (with-temp-buffer
        (call-process "eix" nil t nil "--print-all-useflags")
        (buffer-string)))))

(defvar helm-system-packages-portage-use-source
  (helm-build-in-buffer-source "USE flags"
    :init 'helm-system-packages-portage-use-init
    :candidate-transformer 'helm-system-packages-portage-highlight
    :action '(("Description" .
               (lambda (elm)
                 (switch-to-buffer helm-system-packages-buffer)
                 (erase-buffer)
                 (apply #'call-process "euse" nil t nil `("-i" ,elm))
                 (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                 (font-lock-mode 1)))
              ("Enable" .
               (lambda (_)
                 (helm-system-packages-run-as-root "euse" "-E")))
              ("Disable" .
               (lambda (_)
                 (helm-system-packages-run-as-root "euse" "-D")))
              ("Remove" .
               (lambda (_)
                 (helm-system-packages-run-as-root "euse" "-P")))
              ("Show which dependencies use this flag" .
               (lambda (_)
                 (helm-system-packages-print "equery" "-C" "h"))))))

;; TODO: Factor with helm-system-packages-highlight?
(defun helm-system-packages-portage-highlight (use-flags)
  "Highlight enabled USE flags."
  (let ((local-uses (split-string (with-temp-buffer
                                    (call-process "portageq" nil t nil "envvar" "USE")
                                    (buffer-string)))))
    (mapcar (lambda (use-flag)
              (propertize use-flag 'face
                          (when (member use-flag local-uses)
                            'helm-system-packages-explicit)))
            use-flags)))

(defun helm-system-packages-portage ()
  "Preconfigured `helm' for Portage."
  (helm-other-buffer '(helm-system-packages-portage-source
                       helm-system-packages-portage-use-source)
                     "*helm portage*"))

(provide 'helm-system-packages-portage)

;;; helm-system-packages-portage.el ends here

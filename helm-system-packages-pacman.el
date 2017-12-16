;;; helm-system-packages-pacman.el --- Helm UI for Arch Linux' pacman. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017        Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.6.9
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
;; Helm UI for Arch Linux' pacman.

;;; Code:
(require 'helm)
(require 'helm-system-packages)

(defun helm-system-packages-pacman-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (call-process "pacman" nil t nil "--query" "--explicit" "--quiet")
                  (buffer-string))))

(defun helm-system-packages-pacman-list-dependencies ()
  "List packages installed as a dependency."
  (split-string (with-temp-buffer
                  (call-process "pacman" nil t nil "--query" "--deps" "--quiet")
                  (buffer-string))))

(defun helm-system-packages-pacman-list-all ()
  "List all packages."
  (sort
   (split-string (with-temp-buffer
                   (call-process "pacman" nil t nil "--sync" "--search" "--quiet")
                   (buffer-string)))
   'string<))

(defun helm-system-packages-pacman-list-descriptions ()
  "Cache all package descriptions."
  (with-temp-buffer
    ;; TODO: Output to Elisp?
    (call-process "expac" nil '(t nil) nil "--sync" "%n: %d")
    (let (descs)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\): \\(.*\\)" nil t)
        (push (cons (intern (match-string 1)) (match-string 2)) descs))
      descs)))

(defvar helm-system-packages-pacman-source
  (helm-build-in-buffer-source "pacman source"
    :init 'helm-system-packages-init
    :candidate-transformer 'helm-system-packages-highlight
    :candidate-number-limit 1000
    :action '(("Show package(s)" .
               (lambda (_)
                 (helm-system-packages-print "pacman" "--sync" "--info" "--info")))
              ("Install" .
               (lambda (_)
                 (helm-system-packages-run-as-root "pacman" "--sync" "--needed")))
              ("Uninstall" .
               (lambda (_)
                 (helm-system-packages-run-as-root "pacman" "--remove" "--recursive")))
              ("Find files" .
               ;; TODO: pacman supports querying files of non-installed packages.  This is slower though.
               ;; pacman --files --list --quiet
               ;; Note: Must add the "/" prefix manually.
               (lambda (_)
                 (helm-system-packages-find-files "pacman" "--query" "--list" "--quiet")))
              ("Show dependencies" .
               (lambda (_)
                 ;; TODO: As an optimization, --query could be used and --sync could be a fallback.
                 (helm-system-packages-print "expac" "--sync" "--listdelim" "\n" "%E")))
              ("Show reverse dependencies" .
               (lambda (_)
                 (helm-system-packages-print "expac" "--sync" "--listdelim" "\n" "%N")))
              ("Browse homepage URL" .
               (lambda (_)
                 (helm-system-packages-print "expac" "--sync" "%u")))
              ("Refresh" . helm-system-packages-refresh))))

(defun helm-system-packages-pacman ()
  "Preconfigured `helm' for pacman."
  (helm :sources '(helm-system-packages-pacman-source)
        :buffer "*helm pacman*"
        :truncate-lines t
        :input (substring-no-properties (or (thing-at-point 'symbol) ""))))

(provide 'helm-system-packages-pacman)

;;; helm-system-packages-pacman.el ends here

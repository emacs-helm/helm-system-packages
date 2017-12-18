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


(defface helm-system-packages-pacman-explicit '((t (:inherit font-lock-warning-face)))
  "Face for explicitly installed packages."
  :group 'helm-system-packages)

(defface helm-system-packages-pacman-dependencies '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for packages installed as dependencies."
  :group 'helm-system-packages)

(defface helm-system-packages-pacman-orphans '((t (:inherit font-lock-string-face :slant italic)))
  "Face for orphan packages (unrequired dependencies)."
  :group 'helm-system-packages)

(defface helm-system-packages-pacman-locals '((t (:weight bold)))
  "Face for local packages."
  :group 'helm-system-packages)

(defvar helm-system-packages-pacman--all nil
  "String of all packages.")

(defvar helm-system-packages-pacman--descriptions nil
  "String of all packages with their description.")

(defun helm-system-packages-pacman-list-explicit ()
  "List explicitly installed packages."
  (split-string (with-temp-buffer
                  (call-process "pacman" nil t nil "--query" "--explicit" "--quiet")
                  (buffer-string))))

(defun helm-system-packages-pacman-list-dependencies ()
  "List packages installed as a required dependency."
  (split-string (with-temp-buffer
                  (call-process "pacman" nil t nil "--query" "--deps" "--quiet")
                  (buffer-string))))

(defun helm-system-packages-pacman-list-orphans ()
  "List orphan packages (unrequired dependencies)."
  (split-string (with-temp-buffer
                  (call-process "pacman" nil t nil "--query" "--deps" "--unrequired" "--quiet")
                  (buffer-string))))

(defun helm-system-packages-pacman-list-locals ()
  "List explicitly installed local packages.
Local packages can also be orphans, explicit or dependencies."
  (split-string (with-temp-buffer
                  (call-process "pacman" nil t nil "--query" "--foreign" "--quiet")
                  (buffer-string))))

;; TODO: Possible optimization: Re-use helm-system-packages-pacman-list-descriptions.
(defun helm-system-packages-pacman-buffer-all ()
  "Cache all package descriptions."
  (with-temp-buffer
    (call-process "expac" nil '(t nil) nil "--sync" "%n")
    (apply 'call-process "expac" nil '(t nil) nil "--query" "%n" (helm-system-packages-pacman-list-locals))
    (sort-lines nil (point-min) (point-max))
    (buffer-string)))

(defcustom helm-system-packages-pacman-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap.")

;; TODO: Possible optimization: Re-use helm-system-packages-pacman-list-locals.
(defun helm-system-packages-pacman-buffer-descriptions ()
  "Cache all package descriptions."
  (with-temp-buffer
    ;; TODO: Possible optimization: Output directly in Elisp?
    (let ((format-string (format "%%-%dn  %%d" helm-system-packages-pacman-column-width)))
      (call-process "expac" nil '(t nil) nil "--sync" format-string)
      (apply 'call-process "expac" nil '(t nil) nil "--query" format-string (helm-system-packages-pacman-list-locals)))
    (sort-lines nil (point-min) (point-max))
    (buffer-string)))

(defun helm-system-packages-pacman-init ()
  "Cache package lists and create Helm buffer."
  (unless (and helm-system-packages-pacman--all helm-system-packages-pacman--descriptions)
    (helm-system-packages-pacman-refresh))
  ;; TODO: We should only create the buffer if it does not already exist.
  ;; On the other hand, we need to be able to override the package list.
  ;; (unless (helm-candidate-buffer) ...
  (helm-init-candidates-in-buffer
      'global
    (if helm-system-packages-details-flag
        helm-system-packages-pacman--descriptions
      helm-system-packages-pacman--all)))

(defun helm-system-packages-pacman-refresh ()
  "Refresh the package list."
  (interactive)
  (setq helm-system-packages-pacman--descriptions (helm-system-packages-pacman-buffer-descriptions)
        helm-system-packages-pacman--all (helm-system-packages-pacman-buffer-all))
  (let ((explicit (helm-system-packages-pacman-list-explicit))
         (dependencies (helm-system-packages-pacman-list-dependencies))
         (orphans (helm-system-packages-pacman-list-orphans))
         (locals (helm-system-packages-pacman-list-locals)))
    (setq helm-system-packages--display-lists nil)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-pacman-explicit)) helm-system-packages--display-lists))
    (dolist (p dependencies)
      (push (cons p '(helm-system-packages-pacman-dependencies)) helm-system-packages--display-lists))
    (dolist (p orphans)
      (push (cons p '(helm-system-packages-pacman-orphans)) helm-system-packages--display-lists))
    (dolist (p locals)
      ;; Local packages are necessarily either explicitly installed or a required dependency or an orphan.
      (push 'helm-system-packages-pacman-locals (cdr (assoc p helm-system-packages--display-lists))))))

(defvar helm-system-packages-pacman-source
  (helm-build-in-buffer-source "pacman source"
    :init 'helm-system-packages-pacman-init
    :candidate-transformer 'helm-system-packages-highlight
    :candidate-number-limit helm-system-packages-candidate-limit
    :display-to-real 'helm-system-packages-extract-name
    :action '(("Show package(s)" .
               (lambda (_)
                 (helm-system-packages-print "pacman" "--sync" "--info" "--info")))
              ("Install (`C-u' to reinstall)" .
               (lambda (_)
                 (helm-system-packages-run-as-root "pacman" "--sync" (unless helm-current-prefix-arg "--needed"))))
              ("Uninstall (`C-u' to include dependencies)" .
               (lambda (_)
                 (helm-system-packages-run-as-root "pacman" "--remove" (when helm-current-prefix-arg "--recursive"))))
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
              ("Mark as dependency" .
               (lambda (_)
                 (helm-system-packages-run-as-root "pacman" "--database" "--asdeps")))
              ("Mark as explicit" .
               (lambda (_)
                 (helm-system-packages-run-as-root "pacman" "--database" "--asexplicit")))
              ("Browse homepage URL" .
               (lambda (_)
                 (helm-system-packages-browse-url (split-string (helm-system-packages-run "expac" "--sync" "%u") "\n" t)))))))

(defun helm-system-packages-pacman ()
  "Preconfigured `helm' for pacman."
  (helm :sources '(helm-system-packages-pacman-source)
        :buffer "*helm pacman*"
        :truncate-lines t
        :input (substring-no-properties (or (thing-at-point 'symbol) ""))))

(provide 'helm-system-packages-pacman)

;;; helm-system-packages-pacman.el ends here

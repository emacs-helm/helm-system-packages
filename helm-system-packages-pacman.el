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

(defvar helm-system-packages-pacman-help-message
  "* Helm pacman

Requirements:

- pacman
- expac

** Commands
\\<helm-system-packages-pacman-map>
\\[helm-system-packages-pacman-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-pacman-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-pacman-toggle-dependencies]\t\tToggle display of required dependencies.
\\[helm-system-packages-pacman-toggle-orphans]\t\tToggle display of unrequired dependencies.
\\[helm-system-packages-pacman-toggle-locals]\t\tToggle display of local packages.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-pacman-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-pacman-toggle-explicit)
    (define-key map (kbd "M-U")   'helm-system-packages-pacman-toggle-uninstalled)
    (define-key map (kbd "M-D")   'helm-system-packages-pacman-toggle-dependencies)
    (define-key map (kbd "M-O")   'helm-system-packages-pacman-toggle-orphans)
    (define-key map (kbd "M-L")   'helm-system-packages-pacman-toggle-locals)
    (define-key map (kbd "C-]")   'helm-system-packages-toggle-descriptions)
    map))

(defvar helm-system-packages-pacman--show-uninstalled-p t)
(defvar helm-system-packages-pacman--show-explicit-p t)
(defvar helm-system-packages-pacman--show-dependencies-p t)
(defvar helm-system-packages-pacman--show-orphans-p t)
(defvar helm-system-packages-pacman--show-locals-p t)

(defun helm-system-packages-pacman-toggle-explicit ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-pacman--show-explicit-p (not helm-system-packages-pacman--show-explicit-p))
    (helm-update)))
(put 'helm-system-packages-pacman-toggle-explicit 'helm-only t)

(defun helm-system-packages-pacman-toggle-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-pacman--show-uninstalled-p (not helm-system-packages-pacman--show-uninstalled-p))
    (helm-update)))
(put 'helm-system-packages-pacman-toggle-uninstalled 'helm-only t)

(defun helm-system-packages-pacman-toggle-dependencies ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-pacman--show-dependencies-p (not helm-system-packages-pacman--show-dependencies-p))
    (helm-update)))
(put 'helm-system-packages-pacman-toggle-dependencies 'helm-only t)

(defun helm-system-packages-pacman-toggle-orphans ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-pacman--show-orphans-p (not helm-system-packages-pacman--show-orphans-p))
    (helm-update)))
(put 'helm-system-packages-pacman-toggle-orphans 'helm-only t)

(defun helm-system-packages-pacman-toggle-locals ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-pacman--show-locals-p (not helm-system-packages-pacman--show-locals-p))
    (helm-update)))
(put 'helm-system-packages-pacman-toggle-locals 'helm-only t)

;; TODO: Propertize the cache directly?
(defun helm-system-packages-pacman-transformer (packages)
  ;; TODO: Possible optimization: Get rid of `reverse'.
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p) helm-system-packages--display-lists))))
        (cond
         ((not face) (when helm-system-packages-pacman--show-uninstalled-p (push p res)))
         ;; For filtering, we consider local packages and non-local packages
         ;; separately, thus we need to treat local packages first.
         ;; TODO: Add support for multiple faces.
         ((memq 'helm-system-packages-pacman-locals face)
          (when helm-system-packages-pacman--show-locals-p (push (propertize p 'face (car face)) res)))
         ((or
           (and helm-system-packages-pacman--show-explicit-p (memq 'helm-system-packages-pacman-explicit face))
           (and helm-system-packages-pacman--show-dependencies-p (memq 'helm-system-packages-pacman-dependencies face))
           (and helm-system-packages-pacman--show-orphans-p (memq 'helm-system-packages-pacman-orphans face)))
          (push (propertize p 'face (car face)) res)))))))

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

(defvar helm-system-packages-pacman--names nil
  "Cache of all packages.")

(defvar helm-system-packages-pacman--descriptions nil
  "Cache of all package names with descriptions.")

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
(defun helm-system-packages-pacman-cache-names ()
  "Cache all package names."
  (with-temp-buffer
    (call-process "expac" nil '(t nil) nil "--sync" "%n")
    (apply 'call-process "expac" nil '(t nil) nil "--query" "%n" (helm-system-packages-pacman-list-locals))
    (sort-lines nil (point-min) (point-max))
    (buffer-string)))

(defcustom helm-system-packages-pacman-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap."
  :group 'helm-system-packages
  :type 'integerp)

;; TODO: Possible optimization: Re-use helm-system-packages-pacman-list-locals.
(defun helm-system-packages-pacman-cache-descriptions ()
  "Cache all package names with descriptions."
  (with-temp-buffer
    ;; TODO: Possible optimization: Output directly in Elisp?
    (let ((format-string (format "%%-%dn  %%d" helm-system-packages-pacman-column-width)))
      (call-process "expac" nil '(t nil) nil "--sync" format-string)
      (apply 'call-process "expac" nil '(t nil) nil "--query" format-string (helm-system-packages-pacman-list-locals)))
    (sort-lines nil (point-min) (point-max))
    (buffer-string)))

(defun helm-system-packages-pacman-init ()
  "Cache package lists and create Helm buffer."
  (unless (and helm-system-packages-pacman--names helm-system-packages-pacman--descriptions)
    (helm-system-packages-pacman-refresh))
  ;; TODO: We should only create the buffer if it does not already exist.
  ;; On the other hand, we need to be able to override the package list.
  ;; (unless (helm-candidate-buffer) ...
  (helm-init-candidates-in-buffer
      'global
    (if helm-system-packages-show-descriptions-p
        helm-system-packages-pacman--descriptions
      helm-system-packages-pacman--names)))

(defun helm-system-packages-pacman-refresh ()
  "Refresh the package list."
  (interactive)
  (setq helm-system-packages-pacman--descriptions (helm-system-packages-pacman-cache-descriptions)
        helm-system-packages-pacman--names (helm-system-packages-pacman-cache-names))
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

(defcustom helm-system-packages-pacman-confirm-p t
  "Prompt for confirmation before proceding with transaction."
  :group 'helm-system-packages
  :type 'boolean)

(defun helm-system-packages-pacman-info (_candidate)
  "Print information about the selected packages.

The local database will be queried if possible, while the sync database is used as a fallback.
Note that they don't hold the exact same information.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  ;; TODO: Sort buffer output? Or keep the mark order?
  (let ((candidates (reverse (helm-marked-candidates)))
        local local-res
        sync sync-res
        ;; Record the current prefix arg now because we will change buffer later.
        (prefix helm-current-prefix-arg))
    (dolist (c candidates)
      (push c (if (assoc c helm-system-packages--display-lists) local sync)))
    (if (and (not local) (not sync))
        (message "No result")
      ;; Get local candidates first, then sync candidates.
      ;; TODO: Write directly to Org buffer.
      (when local
        (setq local-res
              (with-temp-buffer
                (apply #'call-process "pacman" nil t nil "--query" "--info" "--info" local)
                (buffer-string)))
        (unless prefix
          (switch-to-buffer helm-system-packages-buffer)
          (erase-buffer)
          (org-mode)
          (view-mode 0)
          (setq local-res (replace-regexp-in-string "\\`.*: " "* " local-res))
          (setq local-res (replace-regexp-in-string "\n\n.*: " "\n* " local-res)))
        (save-excursion (insert local-res)))
      (when sync
        (setq sync-res
              (with-temp-buffer
                (apply #'call-process "pacman" nil t nil "--sync" "--info" "--info" sync)
                (buffer-string)))
        (unless prefix
          (switch-to-buffer helm-system-packages-buffer)
          (unless local-res
            (erase-buffer)
            (org-mode))
          (view-mode 0)
          ;; `pacman -Sii' returns:
          ;;
          ;; Repository      : community
          ;; Name            : FOO
          ;;
          ;; We need to remove the second line and print `* FOO'.
          (setq sync-res (replace-regexp-in-string "\\`\\(.*\\)\n.*: \\(.*\\)" "* \\2\n\\1" sync-res))
          (setq sync-res (replace-regexp-in-string "\n\n\\(.*\\)\n.*: \\(.*\\)" "\n* \\2\n\\1" sync-res)))
        (save-excursion (insert sync-res)))
      (unless prefix
        (save-mark-and-excursion
         (push-mark (point-max) nil t)
         (goto-char (point-min))
         (org-sort-entries nil ?a)))
      (unless (or helm-current-prefix-arg helm-system-packages-editable-info-p)
        (view-mode 1)))))

(defun helm-system-packages-pacman-find-files (_candidate)
  (require 'helm-files)
  (let ((candidates (reverse (helm-marked-candidates)))
        local sync res)
    (dolist (c candidates)
      (push c (if (assoc c helm-system-packages--display-lists) local sync)))
    (if (and (not local) (not sync))
        (message "No result")
      (setq res
            (with-temp-buffer
              ;; pacman's file database queries do not include the leading '/'.
              (when sync
                ;; TODO: Check for errors when file database does not exist.
                (apply #'call-process "pacman" nil t nil "--files" "--list" "--quiet" sync)
                (goto-char (point-min))
                (while (not (eobp))
                  (insert "/")
                  (forward-line)))
              (when local
                (apply #'call-process "pacman" nil t nil "--query" "--list" "--quiet" local))
              (buffer-string)))
      (if helm-current-prefix-arg
          (insert res)
        (helm :sources (helm-build-sync-source "Package files"
                         :candidates (split-string res "\n")
                         :candidate-transformer (lambda (files)
                                                  (let ((helm-ff-transformer-show-only-basename nil))
                                                    (mapcar 'helm-ff-filter-candidate-one-by-one files)))
                         :candidate-number-limit 'helm-ff-candidate-number-limit
                         :persistent-action 'helm-find-files-persistent-action
                         :keymap 'helm-find-files-map
                         :action 'helm-find-files-actions)
              :buffer "*helm system package files*")))))

(defcustom helm-system-packages-pacman-actions
  '(("Show package(s)" . helm-system-packages-pacman-info)
    ("Install (`C-u' to reinstall)" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--sync" (unless helm-current-prefix-arg "--needed") (unless helm-system-packages-pacman-confirm-p "--noconfirm"))))
    ("Uninstall (`C-u' to include dependencies)" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--remove" (when helm-current-prefix-arg "--recursive") (unless helm-system-packages-pacman-confirm-p "--noconfirm"))))
    ("Find files" . helm-system-packages-pacman-find-files)
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
       (helm-system-packages-browse-url (split-string (helm-system-packages-run "expac" "--sync" "%u") "\n" t)))))
    "Actions for Helm pacman."
    :group 'helm-system-packages
    :type '(alist :key-type string :value-type function))

(defvar helm-system-packages-pacman-source
  (helm-build-in-buffer-source "pacman source"
    :init 'helm-system-packages-pacman-init
    :candidate-transformer 'helm-system-packages-pacman-transformer
    :candidate-number-limit helm-system-packages-candidate-limit
    :display-to-real 'helm-system-packages-extract-name
    :keymap helm-system-packages-pacman-map
    :help-message 'helm-system-packages-pacman-help-message
    :persistent-help "Show package description"
    :action helm-system-packages-pacman-actions))

(defun helm-system-packages-pacman ()
  "Preconfigured `helm' for pacman."
  (helm :sources '(helm-system-packages-pacman-source)
        :buffer "*helm pacman*"
        :truncate-lines t
        :input (when helm-system-packages-use-symbol-at-point-p
                 (substring-no-properties (or (thing-at-point 'symbol) "")))))

(provide 'helm-system-packages-pacman)

;;; helm-system-packages-pacman.el ends here

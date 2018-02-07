;;; helm-system-packages-pacman.el --- Helm UI for Arch Linux' pacman. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
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
;; Helm UI for Arch Linux' pacman.

;;; Code:
(require 'helm)
(require 'helm-system-packages)

(declare-function org-sort-entries "org.el")

(defvar helm-system-packages-pacman-help-message
  "* Helm pacman

** Commands
\\<helm-system-packages-pacman-map>
\\[helm-system-packages-pacman-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-pacman-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-pacman-toggle-dependencies]\t\tToggle display of required dependencies.
\\[helm-system-packages-pacman-toggle-orphans]\t\tToggle display of unrequired dependencies.
\\[helm-system-packages-pacman-toggle-locals]\t\tToggle display of local packages.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-pacman-map
  ;; M-U is reserved for `helm-unmark-all'.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-pacman-toggle-explicit)
    (define-key map (kbd "M-N")   'helm-system-packages-pacman-toggle-uninstalled)
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

(defvar helm-system-packages-pacman--virtual-list nil
  "List of virtual packages.
This is only used for dependency display.")

;; TODO: Propertize the cache directly?
(defun helm-system-packages-pacman-transformer (packages)
  ;; TODO: Possible optimization: Get rid of `reverse'.
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p) helm-system-packages--display-lists))))
        (cond
         ((and (not face) (member (helm-system-packages-extract-name p) helm-system-packages-pacman--virtual-list))
          ;; When displaying dependencies, package may be virtual.
          ;; Check first since it is also an "uninstalled" package.
          (push (propertize p 'face 'helm-system-packages-pacman-virtual) res))
         ((and (not face) helm-system-packages-pacman--show-uninstalled-p)
               (push p res))
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

(defface helm-system-packages-pacman-virtual '((t (:slant italic)))
  "Face for virtual packages."
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

(defcustom helm-system-packages-pacman-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap."
  :group 'helm-system-packages
  :type 'integer)

(defun helm-system-packages-pacman-cache (local-packages)
  "Cache all package names with descriptions.
LOCAL-PACKAGES is a list of strings.
Return (NAMES . DESCRIPTIONS), a cons of two strings."
  ;; We build both caches at the same time.  We could also build just-in-time, but
  ;; benchmarks show that it only saves less than 20% when building one cache.
  (let (names descriptions)
    (setq descriptions
          (with-temp-buffer
            ;; TODO: Possible optimization: Output directly in Elisp?
            (let ((format-string (format "%%-%dn  %%d" helm-system-packages-pacman-column-width)))
              (call-process "expac" nil '(t nil) nil "--sync" format-string)
              (apply 'call-process "expac" nil '(t nil) nil "--query" format-string local-packages))
            (sort-lines nil (point-min) (point-max))
            (buffer-string)))
    ;; replace-regexp-in-string is faster than mapconcat over split-string.
    (setq names
          (replace-regexp-in-string " .*" "" descriptions))
    (cons names descriptions)))

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
  (let ((explicit (helm-system-packages-pacman-list-explicit))
        (dependencies (helm-system-packages-pacman-list-dependencies))
        (orphans (helm-system-packages-pacman-list-orphans))
        (locals (helm-system-packages-pacman-list-locals)))
    (let ((res (helm-system-packages-pacman-cache locals)))
      (setq helm-system-packages-pacman--names (car res)
            helm-system-packages-pacman--descriptions (cdr res)))
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

(defun helm-system-packages-pacman--run (commands)
  "Run pacman COMMANDS over `helm-marked-candidates'.

COMMANDS are a cons in the form of

  ((\"local-command\" \"args\"...) .
   (\"sync-command\" \"args\"...))

The local/sync command will be run on the candidates which are/are not installed
locally, respectively.

Return (\"local-command-result\" . \"sync-command-result\")."
  (let ((candidates (reverse (helm-marked-candidates)))
        local sync)
    (dolist (c candidates)
      (push c (if (assoc c helm-system-packages--display-lists) local sync)))
    (if (and (not local) (not sync))
        (error "No result")
      (cons
       (with-temp-buffer
         (when local
           ;; We discard errors.
           (apply #'call-process (caar commands) nil t nil (append (cdar commands) local))
           (buffer-string)))
       (with-temp-buffer
         (when sync
           (apply #'call-process (cadr commands) nil t nil (append (cddr commands) sync))
           (buffer-string)))))))

(defun helm-system-packages-pacman-info (_candidate)
  "Print information about the selected packages.

The local database will be queried if possible, while the sync
database is used as a fallback.  Note that they don't hold the
exact same information.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  ;; TODO: Sort buffer output? Or keep the mark order?
  (require 'org)
  (let* ((res (helm-system-packages-pacman--run '(("pacman" "--query" "--info" "--info") .
                                                  ("pacman" "--sync" "--info" "--info"))))
         (local-res (car res))
         (sync-res (cdr res))
         buf)
    (setq buf (with-temp-buffer
                (when local-res
                  ;; We insert a double newline at the beginning so that the
                  ;; regexp-replace works on the first entry as well.
                  (save-excursion (insert "\n\n" local-res))
                  (while (re-search-forward "\n\n.*: " nil t)
                    (replace-match "\n* " nil nil)))
                (goto-char (point-max))
                (delete-blank-lines)
                (when sync-res
                  ;; Process sync candidates separately from local candidates.
                  ;; `pacman -Sii' returns:
                  ;;
                  ;; Repository      : community
                  ;; Name            : FOO
                  ;;
                  ;; We need to remove the second line and print `* FOO' at the top.
                  (save-excursion (insert "\n\n" sync-res))
                  (while (re-search-forward "\n\n\\(.*\\)\n.*: \\(.*\\)" nil t)
                    (replace-match "\n* \\2\n\\1" nil nil)))
                ;; Sort all entries.  org-sort-entries works on the whole buffer
                ;; if it is strictly before the first entry.
                (goto-char (point-min))
                (org-mode)
                (org-sort-entries nil ?a)
                (goto-char (point-min))
                (delete-blank-lines)
                (buffer-string)))
    (if helm-current-prefix-arg
        (insert buf)
      ;; TODO: Name temp buffer to helm-system-packages-buffer.
      (switch-to-buffer helm-system-packages-buffer)
      (view-mode 0)
      (erase-buffer)
      (org-mode)
      (save-excursion (insert buf))
      (unless (or helm-current-prefix-arg helm-system-packages-editable-info-p)
        (view-mode 1)))))

(defun helm-system-packages-pacman-find-files (_candidate)
  ;; TODO: Check for errors when file database does not exist.
  (require 'helm-files)
  (let ((res (helm-system-packages-pacman--run '(("pacman" "--query" "--list") .
                                                 ("pacman" "--files" "--list")))))
    (if helm-current-prefix-arg
        (insert res)
      (let (file-list) ; An alist of (package-name . (files...))
        (with-temp-buffer
          (insert (car res) (or (cdr res) ""))
          (goto-char (point-min))
          (let (pkg pkg-list)
            (while (search-forward " " nil t)
              (setq pkg (buffer-substring-no-properties (line-beginning-position) (1- (point))))
              (setq pkg-list (assoc pkg file-list))
              (unless pkg-list
                (push (setq pkg-list (list pkg)) file-list))
              ;; pacman's file database queries do not include the leading '/'.
              (nconc pkg-list (list (concat (unless (char-equal (char-after (point)) ?/) "/")
                                            (buffer-substring-no-properties (point) (line-end-position)))))
              (forward-line))))
        (helm :sources (mapcar
                        (lambda (pkg)
                          (helm-system-packages-build-file-source (car pkg) (cdr pkg)))
                        file-list)
              :buffer "*helm system package files*")))))

(defvar helm-system-packages--source-name "pacman source")

(defvar helm-system-packages-pacman--descriptions-global nil
  "All descriptions.
Used to restore complete description list when browsing dependencies.")
(defvar helm-system-packages-pacman--names-global nil
  "All names.
Used to restore complete name list when browsing dependencies.")

(defun helm-system-packages-pacman-deps (_candidate &optional reverse)
  "Run a Helm session over the packages returned by COMMAND run over `helm-marked-candidates'.
With prefix argument, insert the output at point.

If REVERSE is non-nil, show reverse dependencies instead."
  (setq helm-system-packages-pacman--descriptions (or helm-system-packages-pacman--descriptions-global helm-system-packages-pacman--descriptions)
        helm-system-packages-pacman--descriptions-global helm-system-packages-pacman--descriptions)
  (setq helm-system-packages-pacman--names (or helm-system-packages-pacman--names-global helm-system-packages-pacman--names)
        helm-system-packages-pacman--names-global helm-system-packages-pacman--names)
  (let* ((format-string (if reverse "%N" (concat "%E" (and helm-current-prefix-arg "%o"))))
         (res (helm-system-packages-pacman--run
               (cons
                (list "expac" "--query" "--listdelim" "\n" format-string)
                (list "expac" "--sync" "--listdelim" "\n" format-string))))
         desc-res)
    ;; TODO: Possible optimization: split-string + sort + del-dups instead of working on buffer.
    (setq res (with-temp-buffer
                (insert
                 (or (car res) "")
                 (or (cdr res) ""))
                (sort-lines nil (point-min) (point-max))
                (delete-duplicate-lines (point-min) (point-max))
                (buffer-string)))
    (if (string= res "")
        (message "No dependencies") ; TODO: Do not quit Helm session.
      (dolist (name (split-string res "\n" t))
        (if (string-match (concat "^" name "  .*$") helm-system-packages-pacman--descriptions)
            (setq desc-res (concat desc-res (match-string 0 helm-system-packages-pacman--descriptions) "\n"))
          (push name helm-system-packages-pacman--virtual-list)
          (setq desc-res (concat desc-res
                                 name
                                 (make-string (- helm-system-packages-pacman-column-width (length name)) ? )
                                 "  <virtual package>"
                                 "\n"))))
      (let ((helm-system-packages-pacman--descriptions desc-res)
            (helm-system-packages-pacman--names res)
            (helm-system-packages--source-name (concat
                                                (if reverse "Reverse deps" "Deps")
                                                " of "
                                                (mapconcat 'identity (helm-marked-candidates) " "))))
        (helm-system-packages-pacman)))))

(defcustom helm-system-packages-pacman-actions
  '(("Show package(s)" . helm-system-packages-pacman-info)
    ("Install (`C-u' to reinstall)" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--sync"
                                         (unless helm-current-prefix-arg "--needed")
                                         (unless helm-system-packages-pacman-confirm-p "--noconfirm"))))
    ("Uninstall (`C-u' to include dependencies)" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--remove"
                                         (when helm-current-prefix-arg "--recursive")
                                         (unless helm-system-packages-pacman-confirm-p "--noconfirm"))))
    ("Browse homepage URL" .
     (lambda (_)
       (helm-system-packages-browse-url (split-string (helm-system-packages-run "expac" "--sync" "%u") "\n" t))))
    ("Find files" . helm-system-packages-pacman-find-files)
    ("Show dependencies (`C-u' to include optional deps)" . helm-system-packages-pacman-deps)
    ("Show reverse dependencies" .
     (lambda (_)
       (helm-system-packages-pacman-deps _ 'reverse)))
    ("Mark as dependency" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--database" "--asdeps")))
    ("Mark as explicit" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--database" "--asexplicit"))))
  "Actions for Helm pacman."
  :group 'helm-system-packages
  :type '(alist :key-type string :value-type function))

(defun helm-system-packages-pacman-build-source ()
  "Build Helm source for pacman."
  (helm-build-in-buffer-source helm-system-packages--source-name
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
  (unless (helm-system-packages-missing-dependencies-p "expac")
    (helm :sources (helm-system-packages-pacman-build-source)
          :buffer "*helm pacman*"
          :truncate-lines t
          :input (when helm-system-packages-use-symbol-at-point-p
                   (substring-no-properties (or (thing-at-point 'symbol) ""))))))

(provide 'helm-system-packages-pacman)

;;; helm-system-packages-pacman.el ends here

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

(defvar helm-system-packages-pacman-help-message
  "* Helm pacman

** Commands
\\<helm-system-packages-pacman-map>
\\[helm-system-packages-pacman-toggle-explicit]\t\tToggle display of explicitly installed packages.
\\[helm-system-packages-pacman-toggle-uninstalled]\t\tToggle display of non-installed.
\\[helm-system-packages-pacman-toggle-dependencies]\t\tToggle display of required dependencies.
\\[helm-system-packages-pacman-toggle-orphans]\t\tToggle display of unrequired dependencies.
\\[helm-system-packages-pacman-toggle-locals]\t\tToggle display of local packages.
\\[helm-system-packages-pacman-toggle-groups]\t\tToggle display of package groups.
\\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

(defvar helm-system-packages-pacman-map
  ;; M-U is reserved for `helm-unmark-all'.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-system-packages-toggle-explicit)
    (define-key map (kbd "M-N")   'helm-system-packages-toggle-uninstalled)
    (define-key map (kbd "M-D")   'helm-system-packages-toggle-dependencies)
    (define-key map (kbd "M-O")   'helm-system-packages-toggle-orphans)
    (define-key map (kbd "M-L")   'helm-system-packages-toggle-locals)
    (define-key map (kbd "M-G")   'helm-system-packages-toggle-groups)
    (define-key map (kbd "C-]")   'helm-system-packages-toggle-descriptions)
    map))

;; TODO: Propertize the cache directly?
(defun helm-system-packages-pacman-transformer (packages)
  ;; TODO: Possible optimization: Get rid of `reverse'.
  (let (res (pkglist (reverse packages)))
    (dolist (p pkglist res)
      (let ((face (cdr (assoc (helm-system-packages-extract-name p) helm-system-packages--display-lists))))
        (cond
         ((and (not face) (member (helm-system-packages-extract-name p) helm-system-packages--virtual-list))
          ;; When displaying dependencies, package may be virtual.
          ;; Check first since it is also an "uninstalled" package.
          (push (propertize p 'face 'helm-system-packages-pacman-virtual) res))
         ((and (not face) helm-system-packages--show-uninstalled-p)
               (push p res))
         ;; For filtering, we consider local packages and non-local packages
         ;; separately, thus we need to treat local packages first.
         ;; TODO: Add support for multiple faces.
         ((memq 'helm-system-packages-locals face)
          (when helm-system-packages--show-locals-p (push (propertize p 'face (car face)) res)))
         ((or
           (and helm-system-packages--show-explicit-p (memq 'helm-system-packages-explicit face))
           (and helm-system-packages--show-dependencies-p (memq 'helm-system-packages-dependencies face))
           (and helm-system-packages--show-orphans-p (memq 'helm-system-packages-orphans face))
           (and helm-system-packages--show-groups-p (memq 'helm-system-packages-groups face)))
          (push (propertize p 'face (car face)) res)))))))

;; TODO: Possible optimization: Split buffer directly.
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

(defun helm-system-packages-pacman-list-groups ()
  "List groups.
Groups can be (un)installed.  Dependency queries list the
packages belonging to the group."
  (split-string (with-temp-buffer
                  (call-process "pacman" nil t nil "--sync" "--groups")
                  (buffer-string))))

(defcustom helm-system-packages-pacman-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap.
If nil, then use `helm-system-package-column-width'."
  :group 'helm-system-packages
  :type 'integer)

(defun helm-system-packages-pacman-cache (local-packages groups)
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
              (call-process "expac" nil '(t nil) nil "--sync" format-string)
              (apply 'call-process "expac" nil '(t nil) nil "--query" format-string local-packages))
            (dolist (g groups)
              (insert (concat g
                              (make-string (- helm-system-packages-column-width (length g)) ? )
                              "  <group>\n")))
            (sort-lines nil (point-min) (point-max))
            (buffer-string)))
    ;; replace-regexp-in-string is faster than mapconcat over split-string.
    (setq names
          (replace-regexp-in-string " .*" "" descriptions))
    (cons names descriptions)))

(defun helm-system-packages-pacman-init ()
  "Cache package lists and create Helm buffer."
  (unless (and helm-system-packages--names helm-system-packages--descriptions)
    (helm-system-packages-pacman-refresh))
  ;; TODO: We should only create the buffer if it does not already exist.
  ;; On the other hand, we need to be able to override the package list.
  ;; (unless (helm-candidate-buffer) ...
  (helm-init-candidates-in-buffer
      'global
    (if helm-system-packages-show-descriptions-p
        helm-system-packages--descriptions
      helm-system-packages--names)))

(defun helm-system-packages-pacman-refresh ()
  "Refresh the package list."
  (interactive)
  (setq helm-system-packages--source-name "pacman source")
  (setq helm-system-packages-column-width
        (or helm-system-packages-pacman-column-width
            helm-system-packages-column-width))
  (let ((explicit (helm-system-packages-pacman-list-explicit))
        (dependencies (helm-system-packages-pacman-list-dependencies))
        (orphans (helm-system-packages-pacman-list-orphans))
        (locals (helm-system-packages-pacman-list-locals))
        (groups (helm-system-packages-pacman-list-groups)))
    (let ((res (helm-system-packages-pacman-cache locals groups)))
      (setq helm-system-packages--names (car res)
            helm-system-packages--descriptions (cdr res)))
    (setq helm-system-packages--display-lists nil)
    (dolist (p explicit)
      (push (cons p '(helm-system-packages-explicit)) helm-system-packages--display-lists))
    (dolist (p dependencies)
      (push (cons p '(helm-system-packages-dependencies)) helm-system-packages--display-lists))
    (dolist (p orphans)
      (push (cons p '(helm-system-packages-orphans)) helm-system-packages--display-lists))
    (dolist (p locals)
      ;; Local packages are necessarily either explicitly installed or a required dependency or an orphan.
      (push 'helm-system-packages-locals (cdr (assoc p helm-system-packages--display-lists))))
    (dolist (p groups)
      (push (cons p '(helm-system-pacman-groups)) helm-system-packages--display-lists))))

(defcustom helm-system-packages-pacman-confirm-p t
  "Prompt for confirmation before proceeding with transaction."
  :group 'helm-system-packages
  :type 'boolean)

(defun helm-system-packages-pacman-info (_candidate)
  "Print information about the selected packages.

The local database will be queried if possible, while the sync
database is used as a fallback.  Note that they don't hold the
exact same information.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (helm-system-packages-show-information
   (helm-system-packages-mapalist
    '((uninstalled (lambda (info-string)
                     ;; Normalize `pacman -Sii' output.", e.g.
                     ;;
                     ;;   Repository      : community
                     ;;   Name            : FOO
                     ;;   ...
                     ;;
                     ;; to
                     ;;
                     ;;   Name            : FOO
                     ;;   Repository      : community
                     ;;   ...
                     (replace-regexp-in-string "\n\n\\(.*\\)\n\\(.*\\)" "\n\n\\2\n\\1"
                                               (concat "\n\n" info-string))))
      (all identity))
    (helm-system-packages-mapalist '((uninstalled (lambda (&rest p) (apply 'helm-system-packages-call '("pacman" "--sync" "--info" "--info") p)))
                                     (groups ignore)
                                     (all (lambda (&rest p) (apply 'helm-system-packages-call '("pacman" "--query" "--info" "--info") p))))
                                   (helm-system-packages-categorize (helm-marked-candidates))))))

(defun helm-system-packages-pacman-find-files (_candidate)
  "List candidate files for display in `helm-system-packages-find-files'.

The local database will be queried if possible, while the sync
database is used as a fallback.  Note that they don't hold the
exact same information."
  ;; TODO: Check for errors when file database does not exist.
  (let ((file-hash (make-hash-table :test 'equal)))
    (dolist (file-string
             (mapcar 'cadr
                     (helm-system-packages-mapalist
                      '((uninstalled (lambda (&rest p)
                                       ;; Prepend the missing leading '/' to pacman's file database queries.'
                                       (replace-regexp-in-string
                                        "\\([^ ]+ \\)" "\\1/"
                                        (apply 'helm-system-packages-call '("pacman" "--files" "--list") p))))
                        (groups ignore)
                        (all (lambda (&rest p)
                               (apply 'helm-system-packages-call '("pacman" "--query" "--list") p))))
                      (helm-system-packages-categorize (helm-marked-candidates)))))
      ;; The first word of the line (package name) is the hash table key,
      ;; the rest is pushed to the value (list of files).
      (string-match "" file-string) ;; Reset search indexes.
      (while (string-match "\n?\\([^ ]+\\) \\(.*\\)" file-string (match-end 0))
        (push (match-string 2 file-string) (gethash (match-string 1 file-string) file-hash))))
    (helm-system-packages-find-files file-hash)))

(defun helm-system-packages-pacman-show-dependencies (_candidate &optional reverse)
  "List candidate dependencies for `helm-system-packages-show-packages'.
If REVERSE is non-nil, list reverse dependencies instead."
  (let ((format-string (if reverse "%N" (concat "%E" (and helm-current-prefix-arg "%o"))))
        (helm-system-packages--source-name (concat
                                            (if reverse "Reverse dependencies" "Dependencies")
                                            " of "
                                            (mapconcat 'identity (helm-marked-candidates) " "))))
    (helm-system-packages-show-packages
     (helm-system-packages-mapalist
      `((uninstalled (lambda (&rest p)
                       (apply 'helm-system-packages-call '("expac" "--sync" "--listdelim" "\n" ,format-string) p)))
        (groups ,(if reverse 'ignore
                   (lambda (&rest p)
                     ;; Warning: "--group" seems to be different from "-g".
                     (apply 'helm-system-packages-call '("expac" "--sync" "-g" "%n") p))))
        (all (lambda (&rest p)
               (apply 'helm-system-packages-call '("expac" "--query" "--listdelim" "\n" ,format-string) p))))
      (helm-system-packages-categorize (helm-marked-candidates))))))

(defun helm-system-packages-pacman-history (_candidate)
  "Filter pacman logs by candidates."
  (switch-to-buffer helm-system-packages-buffer)
  (view-mode 0)
  (erase-buffer)
  (save-excursion (insert-file-contents-literally
                   ;; Find log file location from `pacman' output.
                   (with-temp-buffer
                     (call-process "pacman" nil t nil "--verbose")
                     (goto-char (point-min))
                     (keep-lines "^Log File")
                     (search-forward ":" nil t)
                     (buffer-substring-no-properties (1+ (point)) (line-end-position)))))
  (keep-lines
   (concat "\\[PACMAN\\].*"
           (regexp-opt (helm-marked-candidates)))))

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
    ("Show dependencies (`C-u' to include optional deps)" . helm-system-packages-pacman-show-dependencies)
    ("Show reverse dependencies" .
     (lambda (_)
       (helm-system-packages-pacman-show-dependencies _ 'reverse)))
    ("Mark as dependency" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--database" "--asdeps")))
    ("Mark as explicit" .
     (lambda (_)
       (helm-system-packages-run-as-root "pacman" "--database" "--asexplicit")))
    ("Show history" . helm-system-packages-pacman-history))
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

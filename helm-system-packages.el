;;; helm-system-packages.el --- Helm UI wrapper for system package managers. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2017 ~ 2018 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-system-packages
;; Version: 1.10.2
;; Package-Requires: ((emacs "24.4") (helm "2.8.7") (seq "1.8"))
;; Keywords: helm, packages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Helm UI wrapper for system package managers.

;;; Code:
;;
;; Since package lists can be memory consuming, we use
;; `helm-build-in-buffer-source' to make Helm search faster.
;;
;; The core of the mechanism evolves around the two buffer caches:
;; - the "name" cache with one name per line,
;; - the "description" cache with one "name <whitespace> description" per line.
;;
;; `helm-system-packages-toggle-descriptions' switches from one to the
;; other.
;;
;; The cache is generated by `helm-system-packages-refresh'.
;; How the cache is generated depends on the package manager.  Smart commandline
;; expressions and parsing are crucial to guarantee a swift startup.
;;
;; The Helm transformer filters and fontifies the candidates.
;; `helm-system-packages-refresh' fills "display-lists"
;; with the categories each package belongs to.
;; Categories are really just faces, such as `helm-system-packages-explicit'.
;; No category means the package is not installed.
;;
;; Actions each have their own functions.  helm-system-packages.el provides a
;; lot of helpers.  Key functions include:
;; - `helm-system-packages-categorize' which turns a list of packages (e.g. the
;;   selected candidates) into an alist where keys are categories (without the
;;   "helm-system-packages-" prefix, e.g. "explicit").
;;
;; - `helm-system-packages-mapalist' which allows for chaining function calls
;;   over the aforementioned category alists.

(require 'seq)
(require 'tramp)
(require 'tramp-sh)
(require 'helm)
(require 'cl-lib)
(require 'ansi-color)

(defvar helm-system-packages-shell-buffer-name "helm-system-packages-eshell")
(defvar helm-system-packages-eshell-buffer (concat "*" helm-system-packages-shell-buffer-name "*"))
(make-obsolete-variable 'helm-system-packages-eshell-buffer 'helm-system-packages-shell-buffer-name "1.9.0")
(defvar helm-system-packages-buffer "*helm-system-packages-output*")

(defvar helm-system-packages--show-uninstalled-p t)
(defvar helm-system-packages--show-explicit-p t)
(defvar helm-system-packages--show-dependencies-p t)
(defvar helm-system-packages--show-orphans-p t)
(defvar helm-system-packages--show-locals-p t)
(defvar helm-system-packages--show-groups-p t)
(defvar helm-system-packages--show-pinned-p t)

;; TODO: Replace `mapcar' by `mapcan' when possible.

;; TODO: Possible optimization: turn into hash table, notably the display list.
(defvar helm-system-packages--cache nil
  "Cache of all package names and descriptions.
It's an alist indexed by hostnames.
The values are in the form

  (:names STRING-BUFFER :descriptions STRING-BUFFER :display LIST :title STRING ...)

'display' is a list of

  (package . (faces...))

Optional 'title' is usually the package manager.")

(defvar helm-system-packages--cache-current nil
  "Current host to use from cache.
If nil, use host linked with `default-directory'.")

(defvar helm-system-packages--virtual-list nil
  "List of virtual packages.
This is only used for dependency display.")

(defface helm-system-packages-explicit '((t (:inherit font-lock-warning-face)))
  "Face for explicitly installed packages."
  :group 'helm-system-packages)

(defface helm-system-packages-dependencies '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for packages installed as dependencies."
  :group 'helm-system-packages)

(defface helm-system-packages-orphans '((t (:inherit font-lock-string-face :slant italic)))
  "Face for orphan packages (unrequired dependencies)."
  :group 'helm-system-packages)

(defface helm-system-packages-locals '((t (:weight bold)))
  "Face for local packages."
  :group 'helm-system-packages)

(defface helm-system-packages-groups '((t (:inherit font-lock-doc-face)))
  "Face for package groups."
  :group 'helm-system-packages)

(defface helm-system-packages-pinned '((t (:weight bold)))
  "Face for pinned packages."
  :group 'helm-system-packages)

(defface helm-system-packages-virtual '((t (:slant italic)))
  "Face for virtual packages."
  :group 'helm-system-packages)

(defface helm-system-packages-residuals '((t (:slant italic)))
  "Face for packages with left-over configuration files."
  :group 'helm-system-packages)

;; Shut up byte compiler
(declare-function eshell-interactive-process "esh-cmd.el")
(declare-function eshell-send-input "esh-mode.el")
(defvar eshell-buffer-name)
(defvar helm-ff-transformer-show-only-basename)
(declare-function helm-ff--create-tramp-name "helm-files.el")
(declare-function helm-comp-read "helm-mode.el")
(declare-function org-sort-entries "org.el")
(declare-function helm-system-packages-refresh "helm-system-package.el")
(declare-function outline-show-all "outline.el")

(defun helm-system-packages-toggle-explicit ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-explicit-p (not helm-system-packages--show-explicit-p))
    (helm-update)))
(put 'helm-system-packages-toggle-explicit 'helm-only t)

(defun helm-system-packages-toggle-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-uninstalled-p (not helm-system-packages--show-uninstalled-p))
    (helm-update)))
(put 'helm-system-packages-toggle-uninstalled 'helm-only t)

(defun helm-system-packages-toggle-dependencies ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-dependencies-p (not helm-system-packages--show-dependencies-p))
    (helm-update)))
(put 'helm-system-packages-toggle-dependencies 'helm-only t)

(defun helm-system-packages-toggle-orphans ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-orphans-p (not helm-system-packages--show-orphans-p))
    (helm-update)))
(put 'helm-system-packages-toggle-orphans 'helm-only t)

(defun helm-system-packages-toggle-locals ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-locals-p (not helm-system-packages--show-locals-p))
    (helm-update)))
(put 'helm-system-packages-toggle-locals 'helm-only t)

(defun helm-system-packages-toggle-groups ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-groups-p (not helm-system-packages--show-groups-p))
    (helm-update)))
(put 'helm-system-packages-toggle-groups 'helm-only t)

(defun helm-system-packages-toggle-pinned ()
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages--show-pinned-p (not helm-system-packages--show-pinned-p))
    (helm-update)))
(put 'helm-system-packages-toggle-pinned 'helm-only t)

;; TODO: Don't refresh when eshell-last-command-status is not 0?
(defvar helm-system-packages-refresh nil
  "Function to refresh the package list.
It is called:
- on each session start;
- whenever a shell command completes.")

(defgroup helm-system-packages nil
  "Predefined configurations for `helm-system-packages'."
  :group 'helm)

(defcustom helm-system-packages-column-width 40
  "Column at which descriptions are aligned, excluding a double-space gap."
  :group 'helm-system-packages
  :type 'integer)

(defcustom helm-system-packages-show-descriptions-p t
  "Always show descriptions in package list when non-nil."
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-auto-send-commandline-p t
  "When a transaction commandline is inserted into a shell buffer, "
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-candidate-limit 1000
  "Maximum number of candidates to display at once.
0 means display all."
  :group 'helm-system-packages
  :type 'integer)

(defcustom helm-system-packages-use-symbol-at-point-p nil
  "Whether to use symbol at point as a default search entry."
  :group 'helm-system-packages
  :type 'boolean)

(defcustom helm-system-packages-editable-info-p nil
  "Whether info buffer is editable.
If nil, it is displayed in view-mode, which means \"q\" (default binding) quits
the window."
  :group 'helm-system-packages
  :type 'boolean)

(defun helm-system-packages--cache-get ()
  "Get current cache entry.
See `helm-system-packages--cache-current'."
  (let ((host (or helm-system-packages--cache-current
                  (and (tramp-tramp-file-p default-directory)
                       (tramp-file-name-host (tramp-dissect-file-name default-directory)))
                  "")))
    (cdr (assoc host helm-system-packages--cache))))

(defun helm-system-packages--cache-set (names descriptions display-list &optional title &rest extra)
  "Set current cache entry.
NAMES and DESCRIPTIONS are strings.
TITLE is a string, usually the name of the package manager.

DISPLAY-LIST is a list of (PACKAGE . '(FACES...)).
It associates packages with the list of faces used for display.  A face
corresponds to a category.  A package can belong to multiple
categories (e.g. both \"orphan\" and \"installed\").

EXTRA is an arbitrary prop-val sequence appended to the resulting plist."
  (let ((host (or (and (tramp-tramp-file-p default-directory)
                       (tramp-file-name-host (tramp-dissect-file-name default-directory)))
                  ""))
        (val (append  (list :names names :descriptions descriptions :display display-list :title title) extra)))
    (if (assoc host helm-system-packages--cache)
        (setcdr (assoc host helm-system-packages--cache) val)
      (push (cons host val) helm-system-packages--cache))))

(defun helm-system-packages-init ()
  "Cache package lists and create Helm buffer."
  (let ((val (helm-system-packages--cache-get)))
    (unless val
      (helm-system-packages-refresh)
      (setq val (helm-system-packages--cache-get)))
    ;; TODO: We should only create the buffer if it does not already exist.
    ;; On the other hand, we need to be able to override the package list.
    ;; (unless (helm-candidate-buffer) ...
    (helm-init-candidates-in-buffer
        'global
      (if helm-system-packages-show-descriptions-p
          (plist-get val :descriptions)
        (plist-get val :names)))))

(defun helm-system-packages--make-init (manager)
  "Cache package lists and create Helm buffer."
  (lambda ()
    (let ((val (helm-system-packages--cache-get)))
      (unless val
        (funcall (helm-system-packages-manager-refresh-function manager))
        (setq val (helm-system-packages--cache-get)))
      ;; TODO: We should only create the buffer if it does not already exist.
      ;; On the other hand, we need to be able to override the package list.
      ;; (unless (helm-candidate-buffer) ...
      (helm-init-candidates-in-buffer
          'global
        (if helm-system-packages-show-descriptions-p
            (plist-get val :descriptions)
          (plist-get val :names))))))

(defun helm-system-packages-mapalist (fun-alist alist)
  "Apply each function of FUN-ALIST to the list with the same key in ALIST.
Return the alist of the results.
Keys must be symbols.

The special key `all' matches all members in ALIST.
Only the first match is applied.
If a member of ALIST does not have a matching function, it is dropped.

To explicitly drop an element, use the `ignore' function.
To explicitly keep an element, use the `identity' function."
  (let ((fun-all (car (alist-get 'all fun-alist)))
        (result '()))
    (dolist (e alist)
      (let* ((fun (or (car (alist-get (car e) fun-alist))
                      fun-all))
             (res (and fun (funcall fun (cdr e)))))
        (when res
          (push (cons (car e) res) result))))
    (nreverse result)))

(defun helm-system-packages-categorize (packages)
  "Return an alist of PACKAGES indexed by category.
PACKAGES must be a list.
Order is presever within categories.
Categories are infered from the display list: it's the last word of the first
associated symbol.
If not found, category is `uninstalled'."
  (let ((result '()))
    (dolist (p packages result)
      (let* ((e (assoc p (plist-get (helm-system-packages--cache-get) :display)))
             (category (or (and e (intern (replace-regexp-in-string ".*\\W\\(\\w+\\)$" "\\1" (symbol-name (cadr e)))))
                           'uninstalled))
             (cell (assoc category result)))
        (if cell
            (setcdr cell (nconc (cdr cell) (list p)))
          (push (list category p) result))))))

(defun helm-system-packages-toggle-descriptions ()
  "Toggle description column."
  (interactive)
  (with-helm-alive-p
    (setq helm-system-packages-show-descriptions-p (not helm-system-packages-show-descriptions-p))
    (helm-force-update)))
(put 'helm-system-packages-toggle-descriptions 'helm-only t)

;; TODO: Possible optimization: turn into macro.
(defun helm-system-packages-extract-name (package)
  "Extract package name from the candidate.
This is useful because the Helm session runs over a buffer
source, so there is only a REAL value which might contain
additional display information such as the package description."
  (if helm-system-packages-show-descriptions-p
      (car (split-string package))
    package))

(defun helm-system-packages-show-information (desc-alist)
  "Show package information contained in DESC-ALIST.
DESC-ALIST's keys are ignored, the values are in the form

    ((package-name . package-desc)...)"
  (cond
   ((not desc-alist)
    (message "No information for package(s) %s" (mapconcat 'identity (helm-marked-candidates) " ")))
   ;; TODO: Sort buffer output? Or keep the mark order?
   (helm-current-prefix-arg
    (mapc 'insert (mapcar 'cadr desc-alist)))
   (t (switch-to-buffer helm-system-packages-buffer)
      (view-mode 0)
      (erase-buffer)
      (dolist (desc (sort
                     (apply 'append (mapcar 'cdr desc-alist))
                     (lambda (a b) (string< (car a) (car b)))))
        (insert "* " (car desc) "\n" (replace-regexp-in-string "^* " "- " (cdr desc)) "\n"))
      (goto-char (point-min))
      (org-mode)
      (outline-show-all)
      (unless (or helm-current-prefix-arg helm-system-packages-editable-info-p)
        (view-mode 1)))))

(defun helm-system-packages-call (command &optional args &rest options)
  "COMMAND to run with OPTIONS over the ARGS list.
OPTIONS are insert before ARGS.
Return the result as a string."
  (with-temp-buffer
    ;; We discard errors.
    (apply #'process-file command nil '(t nil) nil (append options args))
    (buffer-string)))

(defun helm-system-packages-run (command &rest args)
  "COMMAND to run over `helm-marked-candidates'."
  (let ((arg-list (append args (helm-marked-candidates))))
    (with-temp-buffer
      ;; We discard errors.
      (apply #'process-file command nil '(t nil) nil arg-list)
      (buffer-string))))
(make-obsolete 'helm-system-packages-run 'helm-system-packages-call "1.9.0")

(defun helm-system-packages-print (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.

With prefix argument, insert the output at point.
Otherwise display in `helm-system-packages-buffer'."
  (let ((res (apply 'helm-system-packages-run (cons command args))))
    (if (string= res "")
        (message "No result")
      (unless helm-current-prefix-arg
        (switch-to-buffer helm-system-packages-buffer)
        (view-mode 0)
        (erase-buffer)
        (org-mode)
        ;; This is does not work for pacman which needs a specialized function.
        (setq res (replace-regexp-in-string "\\`.*: " "* " res))
        (setq res (replace-regexp-in-string "\n\n.*: " "\n* " res))
        (setq res (ansi-color-apply res)))
      (save-excursion (insert res))
      (unless (or helm-current-prefix-arg helm-system-packages-editable-info-p)
        (view-mode 1)))))
(make-obsolete 'helm-system-packages-print 'helm-system-packages-show-information "1.9.0")

(defmacro helm-system-packages-make-tramp-file-name (file)
  "Prefix FILE with path to remote connection.
If local, return FILE unmodified."
  `(let ((v (tramp-dissect-file-name default-directory)))
     ,(if (< emacs-major-version 26)
          `(tramp-make-tramp-file-name
            (tramp-file-name-method v)
            (tramp-file-name-user v)
            (tramp-file-name-host v)
            ,file
            (tramp-file-name-hop v))
        `(tramp-make-tramp-file-name
          (tramp-file-name-method v)
          (tramp-file-name-user v)
          (tramp-file-name-domain v)
          (tramp-file-name-host v)
          (tramp-file-name-port v)
          ,file
          (tramp-file-name-hop v)))))

(defun helm-system-packages-prefix-remote (file)
  (if (tramp-tramp-file-p default-directory)
      (helm-system-packages-make-tramp-file-name file)
    file))

(defun helm-system-packages-build-file-source (package files)
  "Build Helm file source for PACKAGE with FILES candidates.
PACKAGES is a string and FILES is a list of strings."
  (require 'helm-files)
  (helm-build-sync-source (concat package " files")
    :candidates files
    :display-to-real 'helm-system-packages-prefix-remote
    :candidate-number-limit 'helm-ff-candidate-number-limit
    :persistent-action-if 'helm-find-files-persistent-action-if
    :keymap 'helm-find-files-map
    :action 'helm-find-files-actions))

(defun helm-system-packages-find-files (files)
  "Run a `helm-find-files' over files in FILES
FILES are either

- a hash table whose keys are the package names and the values the list of files,
- or a single list of files.

In case of a hash table, one Helm source per package will be created."
  (if (= (hash-table-count files) 0)
      (message "No file list for package(s) %s" (mapconcat 'identity (helm-marked-candidates) " "))
    (require 'helm-files)
    (if (hash-table-p files)
        (let (sources)
          (maphash
           (lambda (package files)
             (push (helm-system-packages-build-file-source package files) sources))
           files)
          (helm :sources sources
                :buffer "*helm system package files*"))
      (helm :sources
            (helm-system-packages-build-file-source "Package files" files)
            :buffer "*helm system package files*"))))

(defun helm-system-packages-files (command &rest args)
  (let ((res (apply #'helm-system-packages-call command args)))
    (if (string= res "")
        (message "No result")
      (if helm-current-prefix-arg
          (insert res)
        (helm :sources
              (helm-system-packages-build-file-source "Packages" (split-string res "\n"))
              :buffer "*helm system package files*")))))
(make-obsolete 'helm-system-packages-files 'helm-system-packages-find-files "1.9.0")

(defun helm-system-packages-shell-name ()
  "Return the name of the shell buffer associated with `default-directory'.
The basename is defined by `helm-system-packages-shell-buffer-name'."
  (concat "*"
          helm-system-packages-shell-buffer-name
          (when (tramp-tramp-file-p default-directory)
            (let ((vec (tramp-dissect-file-name default-directory)))
              (when (or (tramp-file-name-user vec) (tramp-file-name-host vec))
                (concat " "
                        (tramp-file-name-user vec)
                        (and (tramp-file-name-host vec)
                             "@")
                        (tramp-file-name-host vec)))))
          "*"))

(defun helm-system-packages-call-as-root (command args packages)
  "Call COMMAND ARGS PACKAGES as root.
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
        (push "sudo" arg-list)
        (add-hook 'eshell-post-command-hook 'helm-system-packages-refresh nil t)
        (add-hook 'eshell-post-command-hook
                  (lambda () (remove-hook 'eshell-post-command-hook 'helm-system-packages-refresh t))
                  t t)
        (goto-char (point-max))
        (insert (mapconcat 'identity arg-list " "))
        (when helm-system-packages-auto-send-commandline-p
          (eshell-send-input))))))

(defun helm-system-packages-run-as-root (command &rest args)
  "COMMAND to run over `helm-marked-candidates'.
COMMAND will be run in the Eshell buffer `helm-system-packages-eshell-buffer'."
  (helm-system-packages-call-as-root command args (helm-marked-candidates)))

(defun helm-system-packages-run-as-root-over-installed (command &rest args)
  "COMMAND to run over installed packages among `helm-marked-candidates'.
COMMAND will be run in the Eshell buffer `helm-system-packages-eshell-buffer'."
  (helm-system-packages-call-as-root
   command
   args
   (seq-filter (lambda (p) (assoc p (plist-get (helm-system-packages--cache-get) :display)))
               (helm-marked-candidates))))

;; TODO: When all entries are filtered out by the transformer, it seems that
;; bindings don't work (e.g. M-N to re-enable uninstalled packages).  Helm bug?
(defun helm-system-packages-show-packages (pkg-alist &optional title)
  "Run a Helm session over the packages in PACKAGE-ALIST.
The key of the alist is ignored and the package lists are considered as one
single list.  This may change in the future.
The value is a string buffer, like the cache.
TITLE is the name of the Helm session."
  (if (not pkg-alist)
      ;; TODO: Do not quit Helm session.
      (message "No dependency list for package(s) %s" (mapconcat 'identity (helm-marked-candidates) " "))
    ;; TODO: Possible optimization: split-string + sort + del-dups + mapconcat instead of working on buffer.
    (let (desc-res
          (descriptions (plist-get (helm-system-packages--cache-get) :descriptions))
          (buf (with-temp-buffer
                 (mapc 'insert (mapcar 'cdr pkg-alist))
                 (sort-lines nil (point-min) (point-max))
                 (delete-duplicate-lines (point-min) (point-max))
                 (buffer-string))))
      (dolist (name (split-string buf "\n" t))
        (if (string-match (concat "^" name "  .*$") descriptions)
            (setq desc-res (concat desc-res (match-string 0 descriptions) "\n"))
          (push name helm-system-packages--virtual-list)
          (setq desc-res (concat desc-res
                                 name
                                 (make-string (- helm-system-packages-column-width (length name)) ? )
                                 "  <virtual package>"
                                 "\n"))))
      (let ((helm-system-packages--cache-current 'dependencies)
            (ass (assq 'dependencies helm-system-packages--cache))
            (val (list :names buf :descriptions desc-res :title title)))
        (if ass
            (setcdr ass val)
          (push (cons 'dependencies val) helm-system-packages--cache))
        (helm-system-packages)))))

(defun helm-system-packages-browse-url (urls)
  "Browse homepage URLs of `helm-marked-candidates'."
  (if (not urls)
      (message "No result")
    (mapc 'browse-url (helm-comp-read "URL: " urls :must-match t :exec-when-only-one t :marked-candidates t))))

(defun helm-system-packages-missing-dependencies-p (&rest deps)
  "Return non-nil if some DEPS are missing."
  (let ((missing-deps
         (seq-remove (lambda (p)
                       (if (tramp-tramp-file-p default-directory)
                           (tramp-find-executable (tramp-dissect-file-name default-directory) p nil)
                         (executable-find p)))
                     deps)))
    (when missing-deps
      (message "Dependencies are missing (%s), please install them"
               (mapconcat 'identity missing-deps ", ")))))

(cl-defstruct (helm-system-packages-manager
               (:constructor nil)
               (:copier nil)
               (:constructor helm-system-packages-manager-create))
  "Package manager interface.

DEPENDENCIES is a list of strings of external executables
required by the package manager.

HELP-MESSAGE, KEYMAP, TRANSFORMER and ACTIONS are as specified by
`helm-build-in-buffer-source'."
  name
  refresh-function
  dependencies
  ;; Helm source parameters follow:
  help-message
  keymap
  transformer
  actions)

(defun helm-system-packages-build-source (manager)
  "Build Helm source for MANAGER."
  (let ((title (or (plist-get (helm-system-packages--cache-get) :title) "package manager")))
    (helm-build-in-buffer-source title
      :init (helm-system-packages--make-init manager)
      :candidate-transformer (helm-system-packages-manager-transformer manager)
      :candidate-number-limit helm-system-packages-candidate-limit
      :display-to-real 'helm-system-packages-extract-name
      :keymap (helm-system-packages-manager-keymap manager)
      :help-message (helm-system-packages-manager-help-message manager)
      :persistent-help "Show package description"
      :action (helm-system-packages-manager-actions manager))))

;;;###autoload
(defun helm-system-packages (&optional arg)
  "Helm user interface for system packages.
By default choose the package manager dedicated to this system, with a
prefix arg allow choosing package manager"
  (interactive "P")
  ;; Some package managers do not have an executable bearing the same name,
  ;; hence the pair (EXECUTABLE . PACKAGE-MANAGER).
  (let* ((managers '(("emerge" . "portage") ("dnf" . "dnf")
                     ("dpkg" . "dpkg") ("pacman" . "pacman")
                     ("xbps-query" . "xbps") ("brew" . "brew")
                     ;; Keep "guix" last because it can be installed
                     ;; beside other package managers and we want to
                     ;; give priority to the original package
                     ;; manager.
                     ("guix" . "guix")))
         (remote (file-remote-p default-directory))
         (manager (if arg
                      (completing-read "Choose manager: "
                                       (mapcar 'cdr managers))
                    (cl-loop for (exe . mng) in managers thereis
                             (and (executable-find exe remote)
                                  mng)))))
    (cl-assert
     (if arg
         (executable-find (car (rassoc manager managers)) remote)
       manager)
     nil
     (if (eq system-type 'darwin)
         "No supported package manager was found. Check your `exec-path'."
       "No supported package manager was found."))
    (when arg
      (setq helm-system-packages--cache nil
            helm-system-packages--virtual-list nil
            helm-system-packages--cache-current nil))
    (require (intern (concat "helm-system-packages-" manager)))
    (if (boundp (intern (concat "helm-system-packages-" manager)))
        ;; New abstraction.
        (let ((current-manager
               (symbol-value (intern (concat "helm-system-packages-" manager)))))
          (unless (apply 'helm-system-packages-missing-dependencies-p
                         (helm-system-packages-manager-dependencies current-manager))
            (helm :sources (helm-system-packages-build-source current-manager)
                  :buffer (format "*helm %s*" (helm-system-packages-manager-name
                                               current-manager))
                  :truncate-lines t
                  :input (when helm-system-packages-use-symbol-at-point-p
                           (substring-no-properties (or (thing-at-point 'symbol) ""))))))
      ;; Old abstraction.
      (fset 'helm-system-packages-refresh (intern (concat "helm-system-packages-" manager "-refresh")))
      (funcall (intern (concat "helm-system-packages-" manager))))))

(provide 'helm-system-packages)

;;; helm-system-packages.el ends here

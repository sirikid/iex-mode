;;; iex-mode.el --- Comint wrapper around IEx  -*- lexical-binding: t; -*-

;;; Copyright (C) 2020 Ivan Sokolov

;; Author: Ivan Sokolov <ivan-p-sokolov@ya.ru>
;; Version: 0.1.0
;; Keywords: languages, processes
;; Homepage: https://git.sr.ht/~sokolov/iex-mode
;; Package-Requires: ((elixir-mode "2.3.1") (emacs "24.4"))

;;; Commentary:

;;; Code:

(require 'comint)
(require 'elixir-mode)
(require 'subr-x)

(eval-when-compile
  (require 'rx))

(defgroup iex nil
  "Settings and tweaks for `iex-mode'."
  :group 'elixir)

;; TODO: Add iex-restart function
;; (defvar-local iex--buffer-command nil)

(defvar iex-font-lock-keywords
  elixir-font-lock-keywords)

(define-derived-mode iex-mode comint-mode "IEx"
  "🤔"
  :group 'iex
  :syntax-table elixir-mode-syntax-table

  ;; font-lock (cursed)
  (setq-local font-lock-defaults '(iex-font-lock-keywords nil nil nil (font-lock-syntactic-face-function . elixir-font-lock-syntactic-face-function)))

  ;; comment
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)

  ;; syntax
  (setq-local syntax-propertize-function #'elixir-syntax-propertize-function)

  ;; lisp
  (setq-local beginning-of-defun-function #'elixir-beginning-of-defun)
  (setq-local end-of-defun-function #'elixir-end-of-defun)

  ;; comint
  (setq-local comint-prompt-regexp (rx (or "iex" "...") "(" (+ digit) ")>"))

  ;; smie
  (smie-setup elixir-smie-grammar 'verbose-elixir-smie-rules
    :forward-token 'elixir-smie-forward-token
    :backward-token 'elixir-smie-backward-token))

(defun iex--project-dir-read (dir)
  "Read the project directory, default is DIR."
  (read-directory-name "project directory: " dir nil 'mustmatch))

(defun iex--project-dir-topmost (final-dir)
  "Return the topmost directory with mix.exs.
Starts at / and descends to FINAL-DIR."
  (catch 'result
    (let ((default-directory ""))
      (dolist (next (save-match-data (split-string final-dir "/" t)))
        (setq default-directory (concat default-directory "/" next))
        (when (file-exists-p "mix.exs")
          (throw 'result default-directory))))))

(defun iex--project-dir-bottommost (start-dir)
  "Return the bottommost directory with mix.exs.
Starts at START-DIR and ascends to /."
  (catch 'result
    (let ((default-directory start-dir))
      (while (not (file-equal-p "/" default-directory))
        (if (file-exists-p "mix.exs")
            (throw 'result default-directory)
          (setq default-directory (expand-file-name "..")))))))

(defun iex--project-dir-project-first-root (dir)
  "Use built-in libary to find DIR's project and its root.
Requires Emacs 25.1."
  (when (require 'project nil t)
    (let ((current-project (project-current dir)))
      (when current-project
        (car (project-roots current-project))))))

(defun iex--project-dir-projectile-root (dir)
  "Use projectile to find DIR's project and its root.
Requires projectile."
  (when (require 'projectile nil t)
    (projectile-project-root dir)))

(defcustom iex-project-dir-search-startegy '(iex--project-dir-bottommost)
  ""
  :group 'iex
  :type
  '(repeat
    (choice
     (const :tag "Ask user" iex--project-dir-read)
     (const :tag "Topmost with mix.exs" iex--project-dir-topmost)
     (const :tag "Bottommost with mix.exs" iex--project-dir-bottommost)
     (const :tag "project's first root" iex--project-dir-project-first-root)
     (const :tag "projectile's root" iex--project-dir-projectile-root)
     (function :tag "Custom function"))))

(defun iex--project-dir ()
  "Search for project directory."
  (catch 'result
    (dolist (fn iex-project-dir-search-startegy)
      (when-let ((project-dir (funcall fn default-directory)))
        (throw 'result project-dir)))))

;; (defun iex-make-buffer-name (command)
;;   (let ((node (string-match "--s?name[[:space:]]+\\([^[:space:]]+\\)" command))
;;         (s-mix (string-match "-S[[:space:]]+mix" command)))
;;     (concat
;;      "iex"
;;      (when (and s-mix (not (zerop s-mix))
;;                 (require 'projectile nil t)
;;                 (projectile-project-root))
;;        (concat ":" (projectile-project-name)))
;;      (when (and node (not (zerop node)))
;;        (concat "(" (match-string-no-properties 1 command) ")")))))

;;;###autoload
(defun run-iex (command &optional name)
  "Run COMMAND in the current directory in the buffer NAME.
COMMAND can be any, but it makes sense to run `iex'. Default NAME
is `*iex*', it can be overridden with a universal argument."
  (interactive
   (list (read-shell-command "iex> " "iex")
         (if (not current-prefix-arg)
             "iex"
           (read-string "iex buffer name: " "iex"))))

  (let* ((process (or name "iex"))
         (buffer (concat "*" process "*")))
    (pcase (split-string command)
      ('() (user-error "No command supplied"))
      (`(,program . ,switches)
       (unless (get-buffer-process buffer)
         (with-current-buffer
             (apply #'make-comint-in-buffer process buffer program nil switches)
           ;; (setq-local iex--buffer-command command)
           (iex-mode)
           (goto-char (point-max))))
       (pop-to-buffer buffer)))))

;;;###autoload
(defun run-iex-S-mix (project-dir command &optional name)
  "Run COMMAND in the PROJECT-DIR in the buffer NAME."
  (interactive
   (let* ((project-dir (iex--project-dir))
          (default-name (concat "iex:" (file-name-base project-dir))))
     (list
      project-dir
      (read-shell-command "iex> " "iex -S mix")
      (if (not current-prefix-arg) default-name
        (read-string "iex buffer name: " default-name)))))

  (let ((default-directory project-dir))
    (funcall #'run-iex command name)))

(provide 'iex-mode)

;;; iex-mode.el ends here

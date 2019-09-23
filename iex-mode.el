(require 'comint)
(require 'elixir-mode)
(require 'subr-x)

(defgroup iex nil
  "Stub"
  :group 'elixir)

(define-derived-mode iex-mode comint-mode "IEx"
  "🤔"
  :group 'iex
  :syntax-table elixir-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(elixir-font-lock-keywords nil nil nil nil
         (font-lock-syntactic-face-function . elixir-font-lock-syntactic-face-function)))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'syntax-propertize-function)
       #'elixir-syntax-propertize-function)

  (set (make-local-variable 'beginning-of-defun-function)
       #'elixir-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'elixir-end-of-defun)

  (smie-setup elixir-smie-grammar 'verbose-elixir-smie-rules
              :forward-token 'elixir-smie-forward-token
              :backward-token 'elixir-smie-backward-token))

(defun iex-root-default-directory ()
  default-directory)

(defun iex-root-projectile-project-root ()
  (when (require 'projectile nil t)
    (projectile-project-root)))

(defun iex-root-simple ()
  (catch 'result
    (let ((it default-directory))
      (while (not (file-equal-p "/" it))
        (if (file-exists-p (expand-file-name "mix.exs" it))
            (throw 'result it)
          (setq it (expand-file-name ".." it))))
      nil)))

(defcustom iex-default-directory '()
  ""
  :group 'iex
  :type '(list (choice (function :label "Default directory" iex-root-default-directory)
                       (function :label "Projectile project root" iex-root-projectile-project-root)
                       (function :label "Simple" iex-root-simple)
                       (function :label "Function: "))))

(defcustom iex-default-directory-function
  nil
  ""
  :type '(list function))

(defun iex-default-directory ()
  (let ((it iex-default-directory-function))
    (catch 'result
      (while it
        (when-let ((dd (funcall (car it) default-directory)))
          (throw 'result dd))
        (pop it))
      default-directory)))

(defun iex-make-buffer-name (command)
  (let ((node (string-match "--s?name[[:space:]]+\\([^[:space:]]+\\)" command))
        (s-mix (string-match "-S[[:space]]+mix" command)))
    (concat
     "iex"
     (when (and s-mix (not (zerop s-mix))
                (require 'projectile nil t)
                (projectile-project-root))
       (concat ":" (projectile-project-name)))
     (when (and node (not (zerop node)))
       (concat "(" (match-string-no-properties 1 command) ")")))))

(defun run-iex (command)
  (interactive
   (list (read-shell-command "iex> " "iex")))
  (let ((default-directory (iex-default-directory))
        (name (iex-make-buffer-name command)))
    (if-let ((existing (get-buffer (format "*%s*" name))))
        (pop-to-buffer existing)
      (with-current-buffer
          (apply #'make-comint name "sh" nil (list "-c" command))
        (iex-mode)
        (pop-to-buffer (current-buffer))))))

(provide 'iex-mode)

(require 'comint)
(require 'elixir-mode)
(require 'subr-x)

(eval-when-compile
  (require 'projectile))

(defgroup iex nil
  "Stub"
  :group 'elixir)

(defvar-local iex-buffer-command nil)

(defvar iex-mode-map
  (let ((m (make-sparse-keymap)))
    m))

(defvar iex-log-line-re
  "^[[:digit:]]\\{4\\}\\(-[[:digit:]]\\{2\\}\\)\\{2\\}[[:space:]][[:digit:]]\\{2\\}\\(:[[:digit:]]\\{2\\}\\)\\{2\\}\\.[[:digit:]]\\{3\\}"
  ;; (rx line-start (= 4 num) (= 2 (: "-" num num)) space (= 2 num) (= 2 (: ":" num num) "." (= 3 num)))
  )

(defvar iex-match-log
  '(iex-log-line-re 0 font-lock-comment-face))

(defvar iex-font-lock-keywords
  (cons iex-match-log elixir-font-lock-keywords))

(define-derived-mode iex-mode comint-mode "IEx"
  "🤔"
  :group 'iex
  :syntax-table elixir-mode-syntax-table
  (setq-local
   font-lock-defaults
   '(elixir-font-lock-keywords nil nil nil nil (font-lock-syntactic-face-function . elixir-font-lock-syntactic-face-function)))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local syntax-propertize-function #'elixir-syntax-propertize-function)
  (setq-local beginning-of-defun-function #'elixir-beginning-of-defun)
  (setq-local end-of-defun-function #'elixir-end-of-defun)

  (setq-local comint-prompt-regexp "\\(iex\\|\\.\\{3\\}\\)([[:digit:]]+)>")

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
  :type
  '(repeat
    (choice
     (function-item :tag "Default directory" iex-root-default-directory)
     (function-item :tag "Projectile project root" iex-root-projectile-project-root)
     (function-item :tag "Simple" iex-root-simple)
     (function :tag "Function: "))))

(defun iex-default-directory ()
  (let ((it iex-default-directory))
    (catch 'result
      (while it
        (when-let ((dd (funcall (car it))))
          (throw 'result dd))
        (pop it))
      default-directory)))

(defun iex-make-buffer-name (command)
  (let ((node (string-match "--s?name[[:space:]]+\\([^[:space:]]+\\)" command))
        (s-mix (string-match "-S[[:space:]]+mix" command)))
    (concat
     "iex"
     (when (and s-mix (not (zerop s-mix))
                (require 'projectile nil t)
                (projectile-project-root))
       (concat ":" (projectile-project-name)))
     (when (and node (not (zerop node)))
       (concat "(" (match-string-no-properties 1 command) ")")))))

;;;###autoload
(defun run-iex (command)
  (interactive
   (list (read-shell-command "iex> " "iex")))
  (let ((default-directory (iex-default-directory))
        (name (iex-make-buffer-name command))
        (program (car (split-string command)))
        (switches (cdr (split-string command))))
    (with-current-buffer
        (get-buffer-create (concat "*" name "*"))
      (unless (process-live-p name)
        (apply #'make-comint-in-buffer name (current-buffer) program nil switches)
        (setq-local iex-buffer-command command))
      (iex-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'iex-mode)

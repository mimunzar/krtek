(defvar tslime-multiplexer 'tmux)
(defvar tslime-screen-configuration nil)
(defvar tslime-tmux-configuration nil)

(defun tslime-str-join (separator seq)
  (mapconcat 'identity seq separator))

(defun tslime-re-matches (re s)
  (let ((start (string-match re s)))
    (when start (substring s start (match-end 0)))))

(defun tslime-append-newline-if-missing (s)
  (replace-regexp-in-string "\n\n$" "\n" (concat s "\n")))

(defun tslime-paste-buffer-to-tmux-panel (fn-shell buffer panel)
  (funcall fn-shell (tslime-str-join " " (list "tmux -L default load-buffer" buffer)))
  (funcall fn-shell (tslime-str-join " " (list "tmux -L default paste-buffer -d -t" panel))))

(defun tslime-send-string-to-tmux-panel (s conf)
  (with-temp-file (cdr (assoc 'buffer conf)) (insert s))
  (apply 'tslime-paste-buffer-to-tmux-panel (cons 'shell-command (mapcar 'cdr conf))))

(defun tslime-prompt-for-tmux-configuration ()
  (list (cons 'buffer (make-temp-file "tslime-tmux"))
        (cons 'panel (read-string "Type panel id: " "{last}"))))

(defun tslime-send-string-tmux (s)
  (unless tslime-tmux-configuration
    (setq tslime-tmux-configuration (tslime-prompt-for-tmux-configuration)))
  (tslime-send-string-to-tmux-panel s tslime-tmux-configuration))

(defun tslime-paste-buffer-to-screen-panel (fn-shell buffer session region)
  (funcall fn-shell (tslime-str-join " " (list "screen -S" session "-X eval" (concat "'readreg p " buffer "'"))))
  (funcall fn-shell (tslime-str-join " " (list "screen -S" session "-p" region "-X paste p"))))

(defun tslime-send-string-to-screen-panel (s conf)
  (with-temp-file (cdr (assoc 'buffer conf)) (insert s))
  (apply 'tslime-paste-buffer-to-screen-panel (cons 'shell-command (mapcar 'cdr conf))))

(defun tslime-parse-screen-session-list (s)
  (mapcan (lambda (x) (unless (null x) (list x)))
          (mapcar (lambda (x) (tslime-re-matches "[0-9]+\\.[^ \t]+\\.[^ \t]+" x)) (split-string s "\n"))))

(defun tslime-prompt-for-screen-session ()
  (let ((sessions (tslime-parse-screen-session-list (shell-command-to-string "screen -ls"))))
    (if sessions (completing-read "Choose session: " sessions) (user-error "No screen session found"))))

(defun tslime-prompt-for-screen-configuration ()
  (list (cons 'buffer (make-temp-file "tslime-screen"))
        (cons 'session (tslime-prompt-for-screen-session))
        (cons 'region (read-string "Type region id: "))))

(defun tslime-send-string-screen (s)
  (unless tslime-screen-configuration
    (setq tslime-screen-configuration (tslime-prompt-for-screen-configuration)))
  (tslime-send-string-to-screen-panel s tslime-screen-configuration))

(defun tslime-send-string-dispatch (s)
  (pcase tslime-multiplexer
    ('screen (tslime-send-string-screen s))
    ('tmux (tslime-send-string-tmux s))
    (_ (user-error "Unsupported terminal multiplexer %s" tslime-multiplexer))))

(defun tslime-send-region ()
  (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
    (tslime-send-string-dispatch (tslime-append-newline-if-missing s))))

(defun tslime-send ()
  (interactive)
  (let ((pos (point)))
    (unless (use-region-p)
      (mark-paragraph))
    (tslime-send-region)
    (deactivate-mark)
    (goto-char pos)))

(defun tslime-reset ()
  (interactive)
  (setq tslime-screen-configuration nil)
  (setq tslime-tmux-configuration nil))

(global-set-key (kbd "C-c C-c") 'tslime-send)

(provide 'tslime)

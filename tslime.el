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

(defun tslime-paste-buffer-to-tmux-panel (buffer panel fn-shell)
  (funcall fn-shell (tslime-str-join " " (list "tmux -L default load-buffer" buffer)))
  (funcall fn-shell (tslime-str-join " " (list "tmux -L default paste-buffer -d -t" panel))))

(defun tslime-send-string-to-tmux-panel (s panel)
  (let ((s (tslime-append-newline-if-missing s)))
    (with-temp-file tslime-buffer (insert s))
    (tslime-paste-buffer-to-tmux-panel tslime-buffer panel 'shell-command)))

(defun tslime-send-string-tmux (s)
  (unless tslime-buffer
    (setq tslime-buffer (make-temp-file "tslime")))
  (unless tslime-panel
    (setq tslime-panel (read-string "tmux target panel: " "{last}")))
  (tslime-send-string-to-tmux-panel s tslime-panel))

(defun tslime-paste-buffer-to-screen-panel (fn-shell buffer session window)
  (funcall fn-shell (tslime-str-join " " (list "screen -S" session "-X eval" (concat "'readreg p " buffer "'"))))
  (funcall fn-shell (tslime-str-join " " (list "screen -S" session "-p" window "-X paste p"))))

(defun tslime-send-string-to-screen-panel (s conf)
  (let ((s (tslime-append-newline-if-missing s)))
    (with-temp-file (cdr (assoc 'buffer conf)) (insert s))
    (apply 'tslime-paste-buffer-to-screen-panel (cons 'shell-command (mapcar 'cdr conf)))))

(defun tslime-parse-screen-session-list (s)
  (mapcan (lambda (x) (unless (null x) (list x)))
          (mapcar (lambda (x) (tslime-re-matches "[0-9]+\\.[^ \t]+\\.[^ \t]+" x)) (split-string s "\n"))))

(defun tslime-prompt-for-screen-session ()
  (let ((sessions (tslime-parse-screen-session-list (shell-command-to-string "screen -ls"))))
    (if sessions (completing-read "Choose session: " sessions) (user-error "No screen session found"))))

(defun tslime-prompt-for-screen-configuration ()
  `((buffer  . ,(make-temp-file "tslime"))
    (session . ,(tslime-prompt-for-screen-session))
    (window  . ,(read-string "Type window id: "))))

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
    (tslime-send-string-dispatch s)))

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

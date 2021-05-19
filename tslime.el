;; -*- lexical-binding: t -*-
(defvar tslime-multiplexer 'tmux)

(defun tslime-s-join (separator seq)
  (mapconcat 'identity seq separator))

(defun tslime-re-matches (re s)
  (let ((start (string-match re s)))
    (when start (substring s start (match-end 0)))))

(defun tslime-append-newline-if-missing (s)
  (replace-regexp-in-string "\n\n$" "\n" (concat s "\n")))

(defun tslime-paste-buffer-to-tmux-panel (f-shell buffer panel)
  (funcall f-shell (tslime-s-join " " (list "tmux -L default load-buffer" buffer)))
  (funcall f-shell (tslime-s-join " " (list "tmux -L default paste-buffer -d -t" panel))))

(defun tslime-send-to-tmux-panel (s conf)
  (with-temp-file (cdr (assoc 'buffer conf)) (insert s))
  (apply #'tslime-paste-buffer-to-tmux-panel (cons 'shell-command (mapcar 'cdr conf))))

(defun tslime-prompt-for-tmux-conf ()
  (list (cons 'buffer (make-temp-file "tslime-tmux"))
        (cons 'panel (read-string "Type panel id: " "{last}"))))

(defun tslime-paste-buffer-to-screen-panel (f-shell buffer session region)
  (funcall f-shell (tslime-s-join " " (list "screen -S" session "-X eval" (concat "'readreg p " buffer "'"))))
  (funcall f-shell (tslime-s-join " " (list "screen -S" session "-p" region "-X paste p"))))

(defun tslime-send-to-screen-panel (s conf)
  (with-temp-file (cdr (assoc 'buffer conf)) (insert s))
  (apply #'tslime-paste-buffer-to-screen-panel (cons 'shell-command (mapcar 'cdr conf))))

(defun tslime-parse-screen-session-list (s)
  (mapcan (lambda (x) (when x (list x)))
          (mapcar (lambda (x) (tslime-re-matches "[0-9]+\\.[^ \t]+\\.[^ \t]+" x)) (split-string s "\n"))))

(defun tslime-prompt-for-screen-session ()
  (let ((sessions (tslime-parse-screen-session-list (shell-command-to-string "screen -ls"))))
    (if sessions (completing-read "Choose session: " sessions) (user-error "No screen session found"))))

(defun tslime-prompt-for-screen-conf ()
  (list (cons 'buffer (make-temp-file "tslime-screen"))
        (cons 'session (tslime-prompt-for-screen-session))
        (cons 'region (read-string "Type region id: "))))

(defun tslime-make-send-multiplexer (f-prompt-conf f-multiplexer-send)
  (let ((conf nil))
    (lambda (s &optional forget)
      (when (or (not conf) forget) (setq conf (funcall f-prompt-conf)))
      (funcall f-multiplexer-send s conf))))

(defun tslime-make-send-dispatch (f-send-screen f-send-tmux)
  (lambda (multiplexer s &optional forget)
    (pcase multiplexer
      ('screen (funcall f-send-screen s forget))
      ('tmux (funcall f-send-tmux s forget))
      (_ (user-error "Unsupported terminal multiplexer %s" multiplexer)))))

(defconst tslime-send-screen
  (tslime-make-send-multiplexer #'tslime-prompt-for-screen-conf #'tslime-send-to-screen-panel))
(defconst tslime-send-tmux
  (tslime-make-send-multiplexer #'tslime-prompt-for-tmux-conf #'tslime-send-to-tmux-panel))
(defconst tslime-send-dispatch
  (tslime-make-send-dispatch tslime-send-screen tslime-send-tmux))

(defun tslime-send-region (multiplexer &optional forget)
  (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
    (funcall tslime-send-dispatch multiplexer (tslime-append-newline-if-missing s) forget)))

(defun tslime-send (&optional forget)
  (interactive)
  (let ((pos (point)))
    (unless (use-region-p) (mark-paragraph))
    (tslime-send-region tslime-multiplexer forget)
    (deactivate-mark)
    (goto-char pos)))

(defun tslime-forget-send ()
  (interactive)
  (tslime-send 'forget))

(global-set-key (kbd "C-c C-c") #'tslime-send)
(global-set-key (kbd "C-c C-f") #'tslime-forget-send)

(provide 'tslime)

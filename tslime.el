(defvar tslime-multiplexer 'tmux)
(defvar tslime-panel nil)
(defvar tslime-buffer (make-temp-file "tslime"))
(defvar tslime-screen-session nil)

(defun tslime-str-join (separator seq)
  (mapconcat 'identity seq separator))

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
  (unless tslime-panel
    (setq tslime-panel (read-string "tmux target panel: " "{last}")))
  (tslime-send-string-to-tmux-panel s tslime-panel))

(defun tslime-paste-buffer-to-screen-panel (file panel fn-shell)
  (funcall fn-shell (tslime-str-join " " (list "screen -X eval 'readreg p" (concat file "'"))))
  (funcall fn-shell (tslime-str-join " " (list "screen -p" panel "-X paste p"))))

(defun tslime-send-string-to-screen-panel (s panel)
  (let ((s (tslime-append-newline-if-missing s)))
    (with-temp-file tslime-buffer (insert s))
    (tslime-paste-buffer-to-tmux-panel tslime-buffer panel 'shell-command)))

(defun tslime-send-string-screen (s)
  (unless tslime-panel
    (setq tslime-panel (read-string "screen target panel: " "1")))
  (tslime-send-string-to-screen-panel s tslime-panel))

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

(defun tslime-reset-panel ()
  (interactive)
  (setq tslime-panel nil))

(global-set-key (kbd "C-c C-c") 'tslime-send)

(provide 'tslime)

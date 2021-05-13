(defvar krtek-socket nil)
(defvar krtek-panel nil)
(defvar krtek-buffer-file (make-temp-file "emacsKrtek"))

(defun krtek-str-join (separator seq)
  (mapconcat 'identity seq separator))

(defun krtek-append-newline-if-missing (s)
  (replace-regexp-in-string "\n\n$" "\n" (concat s "\n")))

(defun krtek-send-string-to-tmux-panel (s krtek-socket krtek-panel)
  (with-temp-file krtek-buffer-file
    (insert s))
  (shell-command (krtek-str-join " " (list "tmux -L" krtek-socket "load-buffer" krtek-buffer-file)))
  (shell-command (krtek-str-join " " (list "tmux -L" krtek-socket "paste-buffer -d -t" krtek-panel))))

(defun krtek-send-string-tmux (s)
  (unless krtek-socket
    (setq krtek-socket (read-string "tmux socket name or absolute path: " "default")))
  (unless krtek-panel
    (setq krtek-panel (read-string "tmux target panel: " "{last}")))
  (krtek-send-string-to-tmux-panel (krtek-append-newline-if-missing s) krtek-socket krtek-panel))

(defun krtek-send-string-dispatch (s)
  (krtek-send-string-tmux s))

(defun krtek-send-region ()
  (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
    (krtek-send-string-dispatch s)))

(defun krtek-send ()
  (interactive)
  (let ((pos (point)))
    (unless (use-region-p)
      (mark-paragraph))
    (krtek-send-region)
    (deactivate-mark)
    (goto-char pos)))

(defun krtek-reset ()
  (interactive)
  (setq krtek-socket nil)
  (setq krtek-panel nil))

(global-set-key (kbd "C-c C-c") 'krtek-send)


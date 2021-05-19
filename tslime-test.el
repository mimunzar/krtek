(require 'tslime)

(ert-deftest tslime-s-join-test ()
  (should (equal "" (tslime-s-join "" '())))
  (should (equal "foo" (tslime-s-join "x" '("foo"))))
  (should (equal "fooxbar" (tslime-s-join "x" '("foo" "bar")))))

(ert-deftest tslime-re-matches-test ()
  (should (equal nil (tslime-re-matches "turtle" "The quick brown fox")))
  (should (equal "quick" (tslime-re-matches "quick" "The quick brown fox")))
  (should (equal "The quick brown fox" (tslime-re-matches ".*" "The quick brown fox"))))

(ert-deftest tslime-append-newline-if-missing-test ()
  (should (equal "\n" (tslime-append-newline-if-missing "")))
  (should (equal "\n\n" (tslime-append-newline-if-missing "\n\n")))
  (should (equal "\n\nfoo\n" (tslime-append-newline-if-missing "\n\nfoo"))))

(ert-deftest tslime-paste-buffer-to-tmux-panel-test ()
  (let ((result '())
        (fn-shell-command (lambda (x) (push x result))))
    (tslime-paste-buffer-to-tmux-panel fn-shell-command "buffer" "panel")
    (should (equal "tmux -L default paste-buffer -d -t panel" (car result)))
    (should (equal "tmux -L default load-buffer buffer" (cadr result)))))

(ert-deftest tslime-paste-buffer-to-screen-panel-test ()
  (let ((result '())
        (fn-shell-command (lambda (x) (push x result))))
    (tslime-paste-buffer-to-screen-panel fn-shell-command "buffer" "session" "region")
    (should (equal "screen -S session -p region -X paste p" (car result)))
    (should (equal "screen -S session -X eval 'readreg p buffer'" (cadr result)))))

(ert-deftest tslime-parse-screen-session-list-test ()
  (let ((output "No Sockets found in /run/screen/S-mmunzar."))
    (should (equal nil (tslime-parse-screen-session-list output))))
  (let ((output '("There are screens on:"
                  "      27900.pts-20.stolova-hora       (Attached)"
                  "      17403.pts-13.stolova-hora       (Attached)"
                  "2 Sockets in /run/screen/S-mmunzar.")))
    (should (equal '("27900.pts-20.stolova-hora" "17403.pts-13.stolova-hora")
                   (tslime-parse-screen-session-list (tslime-s-join "\n" output))))))

(ert-deftest tslime-make-send-multiplexer-test ()
  (let* ((prompted-times 0)
         (f-prompt-conf (lambda () (setq prompted-times (1+ prompted-times)) "conf"))
         (f-multiplexer-send (lambda (s conf) (list s conf)))
         (f-send-multiplexer (tslime-make-send-multiplexer f-prompt-conf f-multiplexer-send)))
    (should (equal '("foo" "conf") (funcall f-send-multiplexer "foo")))
    (should (equal '("bar" "conf") (funcall f-send-multiplexer "bar")))
    (should (equal 1 prompted-times))
    (should (equal '("baz" "conf") (funcall f-send-multiplexer "baz" 'forget)))
    (should (equal 2 prompted-times))))

(ert-deftest tslime-make-send-dispatch-test ()
  (let* ((f-send-screen (lambda (s &optional forget) (list 'screen s)))
         (f-send-tmux (lambda (s &optional forget) (list 'tmux s)))
         (f-send-dispatch (tslime-make-send-dispatch f-send-screen f-send-tmux)))
    (should (equal '(tmux "foo") (funcall f-send-dispatch 'tmux "foo")))
    (should (equal '(screen "bar") (funcall f-send-dispatch 'screen "bar")))
    (should-error (funcall f-send-dispatch 'wrong "bar"))))

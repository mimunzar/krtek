(ert-deftest krtek-str-join-test ()
  (should (equal "" (krtek-str-join "" '())))
  (should (equal "foo" (krtek-str-join "x" '("foo"))))
  (should (equal "fooxbar" (krtek-str-join "x" '("foo" "bar")))))

(ert-deftest krtek-append-newline-if-missing-test ()
  (should (equal "\n" (krtek-append-newline-if-missing "")))
  (should (equal "\n\n" (krtek-append-newline-if-missing "\n\n")))
  (should (equal "\n\nfoo\n" (krtek-append-newline-if-missing "\n\nfoo"))))

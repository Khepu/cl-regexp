
(defun tokenize (string)
  (loop with mode = nil
        for i below (length string)
        for char = (schar string i)
        for token = (case (car mode)
                      (:escaping
                       (pop mode)
                       (if (and (digit-char-p char)
                                (not (eq :bracket (cadr mode))))
                           (cons :backreference (- (char-code char) (char-code #\0)))
                           (case char
                             (#\t (cons :literal #\tab))
                             (#\n (cons :literal #\newline))
                             (t (cons :literal char)))))
                      (:bracket
                       (case char
                         (#\^ (if (char= #\[ (schar string (1- i)))
                                  :negate
                                  (cons :literal #\^)))
                         (#\\ (push :escaping mode) nil)
                         (#\] (pop mode) :bracket-close)
                         (t (cons :literal char))))
                      (t
                       (case char
                         (#\\ (push :escaping mode) nil)
                         (#\^ :anchor-start)
                         (#\$ :anchor-end)
                         (#\( :group-start)
                         (#\) :group-end)
                         (#\[ (push :bracket mode) :bracket-open)
                         (#\* :kleene)
                         (#\| :either)
                         (#\+ :plus)
                         (#\? :q-mark)
                         (#\. :dot)
                         (t (cons :literal char)))))
        when token
          collect it
        finally (when (eq mode :escaping)
                  (error "Tokenization Error: Dangling backslash!"))))

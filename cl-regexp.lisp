;;(in-package #:cl-regexp)

(defstruct literal
  text)

(defstruct concat
  left
  right)

(defstruct either
  left
  right)

(defstruct repeat
  expr
  min
  max)

(defstruct group
  expr)

(defun tokenize (string)
  (loop with escaping-p = nil
        for char across string
        for token = (if (not escaping-p)
                        (case char
                          (#\\ (setf escaping-p t) nil)
                          (#\( :group-start)
                          (#\) :group-end)
                          (#\* :kleene)
                          (#\| :either)
                          (#\+ :plus)
                          (#\? :q-mark)
                          (#\. :dot)
                          (t (cons :literal char)))
                        (progn
                          (setf escaping-p nil)
                          (if (digit-char-p char)
                              (error "Unsupported") ;;(cons :backreference ....)
                              (case char
                                (#\t (cons :literal #\tab))
                                (#\n (cons :literal #\newline))
                                (t (cons :literal char))))))
        when token
          collect it
        finally (when escaping-p
                  (error "Tokenization Error: Dangling backslash!"))))

(defun make-buffer ()
  (make-array (list 1)
              :element-type 'character
              :adjustable t
              :fill-pointer 0))

(defun finalize-buffer (buffer state)
  (let ((string (when buffer
                  (coerce buffer 'string))))
    (if string
        (let ((literal (make-literal :text string)))
          (typecase state
            (either (if (either-right state)
                        (make-concat :left state
                                     :right literal)
                        (progn
                          (setf (either-right state) literal)
                          state)))
            (null literal)
            (t (make-concat :left state
                            :right literal))))
        state)))

(defun repeat-rightmost-expr (state)
  (loop with current-state = state
        do (typecase current-state
             (concat (if (literal-p (concat-right current-state))
                         (return-from repeat-rightmost-expr
                           (setf (concat-right current-state)
                                 (make-repeat :expr (concat-right current-state))))
                         (setf current-state (concat-right current-state))))
             (either (if (literal-p (either-right current-state))
                         (return-from repeat-rightmost-expr
                           (setf (either-right current-state)
                                 (make-repeat :expr (either-right current-state))))
                         (setf current-state (either-right current-state)))))))

(defun parse (tokens)
  (loop with state = nil
        with groups = nil
        with buffer = nil
        for (current next) on tokens
        ;;do (format t "TOKEN: ~s NEXT: ~s STATE: ~s BUFFER: ~S ~%" current next state buffer)
        do (case (if (consp current)
                     (car current)
                     current)
             (:either (setf state (make-either :left state)))
             (:kleene (let ((repeat (repeat-rightmost-expr state)))
                        (setf (repeat-min repeat) 0)))
             (:q-mark (let ((repeat (repeat-rightmost-expr state)))
                        (setf (repeat-min repeat) 0
                              (repeat-max repeat) 1)))
             (:plus (let ((repeat (repeat-rightmost-expr state)))
                      (setf (repeat-min repeat) 1)))
             (:literal
              (setq current (cdr current))
              (case next
                ;; if :KLEENE follows after a :LITERAL then we need to:
                ;; 1. finalize the existing buffer
                ;; 2. modify the state to reflect the literal added
                ((:kleene :q-mark :plus)
                 (setf state (finalize-buffer buffer state)
                       buffer nil)
                 (let ((literal (make-literal :text (format nil "~a" current))))
                   (setf state
                         (typecase state
                           (either
                            (setf (either-right state)
                                  (if (either-right state)
                                      (make-concat :left (either-right state)
                                                   :right literal)
                                      literal))
                            state)
                           ((or literal repeat concat) (make-concat :left state
                                                                    :right literal))
                           (nil literal)))))

                (:either
                 (unless buffer
                   (setf buffer (make-buffer)))
                 (vector-push-extend current buffer)
                 (setf state (finalize-buffer buffer state)
                       buffer nil))

                (t
                 (unless buffer
                   (setf buffer (make-buffer)))
                 (vector-push-extend current buffer)))))
        finally
           (return (finalize-buffer buffer state))))

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
  expr
  index)

(defstruct backreference
  index)

(defstruct bracket-group
  negate-p
  chars)

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

(defun finalize-bracket (buffer state)
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

(defun repeat-rightmost-expr (state min max)
  (typecase state
    ((or group literal backreference)
     (make-repeat :expr state :min min :max max))
    (t
     (loop with current-state = state
           do (typecase current-state
                (concat (if (or (literal-p (concat-right current-state))
                                (group-p (concat-right current-state))
                                (backreference-p (concat-right current-state)))
                            (progn
                              (setf (concat-right current-state)
                                    (make-repeat :expr (concat-right current-state)
                                                 :min min
                                                 :max max))
                              (return-from repeat-rightmost-expr
                                state))
                            (setf current-state (concat-right current-state))))
                (either (if (or (literal-p (either-right current-state))
                                (group-p (either-right current-state))
                                (backreference-p (either-right current-state)))
                            (progn
                              (setf (either-right current-state)
                                    (make-repeat :expr (either-right current-state)
                                                 :min min
                                                 :max max))
                              (return-from repeat-rightmost-expr
                                state))
                            (setf current-state (either-right current-state)))))))))

(defun parse (tokens)
  (loop with group-index = 1
        with state = nil
        with groups = nil
        with buffer = nil
        for (current next) on tokens
        do (case (if (consp current)
                     (car current)
                     current)
             (:group-start
              (setf state (finalize-buffer buffer state)
                    buffer nil)
              (push state groups)
              (setf state nil)
              (incf group-index))
             (:group-end
              (setf state
                    (let ((context (pop groups))
                          (group (make-group :expr (finalize-buffer buffer state)
                                             :index (decf group-index))))
                      (typecase context
                        (concat (if (concat-right context)
                                    (make-concat :left context :right group)
                                    (setf (concat-right context) group)))
                        (either (if (either-right context)
                                    (make-concat :left context :right group)
                                    (setf (either-right context) group)))
                        (null group)
                        (t (make-concat :left context :right group))))
                    buffer nil))
             (:bracket-open

              )
             (:bracket-close
              (setf state
                    (let ((context (pop groups))
                          (group (make-bracket-group :chars buffer)))
                      (typecase context
                        (concat (if (concat-right context)
                                    (make-concat :left context :right group)
                                    (setf (concat-right context) group)))
                        (either (if (either-right context)
                                    (make-concat :left context :right group)
                                    (setf (either-right context) group)))
                        (null group)
                        (t (make-concat :left context :right group))))
                    buffer nil))
             (:backreference
              (setq current (cdr current))
              (let ((backreference (make-backreference :index current)))
                (setf state
                      (typecase state
                        (concat (if (concat-right state)
                                    (make-concat :left state :right backreference)
                                    (setf (concat-right state) backreference)))
                        (either (if (either-right state)
                                    (make-concat :left state :right backreference)
                                    (setf (either-right state) backreference)))
                        (null backreference)
                        (t (make-concat :left state :right backreference))))))
             (:either (setf state (make-either :left state)))
             (:kleene (setf state (repeat-rightmost-expr state 0 nil)))
             (:q-mark (setf state (repeat-rightmost-expr state 0 1)))
             (:plus   (setf state (repeat-rightmost-expr state 1 nil)))
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

                           ((or literal repeat concat)
                            (make-concat :left state :right literal))

                           (null literal)))))

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

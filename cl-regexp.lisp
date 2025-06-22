;;(in-package #:cl-regexp)

(defstruct literal
  text)

(defstruct concat
  children)

(defstruct either
  left
  right)

(defstruct repeat
  expr
  min
  max)

(defstruct group
  expr ;; in reverse order
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

(defun buffer->literal (buffer)
  (when (plusp (length buffer))
    (make-literal :text (coerce buffer 'string))))

(defun repeat-last (state min max)
  (with-slots (root) state
    (let ((last (pop (concat-children root))))
      (push (make-repeat :expr last
                         :min min
                         :max max)
            (concat-children root)))))

(defstruct state
  (root (make-concat))
  (buffer (make-buffer))
  groups
  (group-index 1))

(defun carry-over-buffer (state)
  (with-slots (root buffer) state
    (let ((literal (buffer->literal buffer)))
      (when literal
        ;; (format t "ADDING LITERAL: ~s~%" literal)
        (push literal (concat-children root))
        (setf buffer (make-buffer))))))

(defun reconstruct (marker state)
  (with-slots (root groups group-index) state
    (loop for (type . group) = (pop groups)
          ;; do (format t "RECONSTRUCT: ~s~%" state)
          when type
            do (case type
                 (:either (setf root (make-either :left group :right root)))
                 (:group  (push (make-group :expr root
                                            :index (decf group-index))
                                (concat-children group))
                  (setf root group)))
          until (eq marker type))))

(defun shelve (state type)
  (with-slots (root groups group-index) state
    (carry-over-buffer state)
    (push (cons type root) groups)
    (setf root (make-concat))
    (when (eq type :group)
      (incf group-index))))

(defun unshelve (state type)
  (carry-over-buffer state)
  (reconstruct type state))

(defun parse (tokens)
  (loop with state = (make-state)
        for (current next) on tokens
        ;; do (format t "MAIN LOOP: ~s~%" state)
        do (case (if (consp current)
                     (car current)
                     current)
             (:group-start (shelve state :group))
             (:group-end   (unshelve state :group))
             (:either      (shelve state :either))
             (:dot    (push :dot (concat-children (state-root state))))
             (:kleene (repeat-last state 0 nil))
             (:q-mark (repeat-last state 0 1))
             (:plus   (repeat-last state 1 nil))
             (:literal
              (setq current (cdr current))
              (case next
                ;; if :KLEENE follows after a :LITERAL then we need to break up
                ;; the string
                ((:kleene :q-mark :plus)
                 (carry-over-buffer state)
                 (push (make-literal :text (format nil "~a" current))
                       (concat-children (state-root state))))
                (t
                 (vector-push-extend current (state-buffer state))))))
        finally
           (carry-over-buffer state)
           (reconstruct nil state)
           (return state)))

(in-package :tic-tac-toe)

(defun get-random-ai-fn ()
  (let ((i (random 2)))
    (case i
      (0 #'make-random-move)
      (1 #'naive-minimax))))

(defun make-random-move (board player)
  (declare (ignorable player))
  (let ((legal-moves (get-legal-moves board)))
    (nth (random (length legal-moves)) legal-moves)))

(defun naive-minimax (board ai-player)
  (let ((win-state (get-win-state ai-player))
        (lose-state (get-win-state (other-player ai-player))))
    (labels
        ((minimax (node maximizing-p)
           (let ((state (get-game-state node)))
             (cond ((eql state win-state) (return-from minimax 1))
                   ((eql state lose-state) (return-from minimax -1))
                   ((eql state +draw+) (return-from minimax 0)))
             (let ((legal-moves (get-legal-moves node))
                   (w-move nil)
                   (w-score (if maximizing-p -2 2))
                   (c-player (if maximizing-p
                                 ai-player
                                 (other-player ai-player)))
                   (comp-fn (if maximizing-p #'> #'<)))
               (loop for move in legal-moves
                     do (make-move node move c-player)
                     do (let ((c-score (minimax node (not maximizing-p))))
                          (when (funcall comp-fn c-score w-score)
                            (setf w-move move
                                  w-score c-score)))
                     do (undo-move node move))
               (values w-score w-move)))))
      (multiple-value-bind (w-score w-move) (minimax board t)
        (declare (ignorable w-score))
        w-move))))

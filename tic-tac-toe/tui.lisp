(in-package :tic-tac-toe)

(defun print-game-board (board)
  (format t "  0 1 2~%")
  (loop for row from 0 below (array-dimension board 0)
        do (format t "~A ~{~A~^|~}~%"
                   row
                   (loop for column from 0 below (array-dimension board 1)
                         collect (or (aref board row column) #\ )))))

(defun print-game-result (state)
  (cond ((eql state +x-won+) (format t "Player X won!~%"))
        ((eql state +o-won+) (format t "Player O won!~%"))
        ((eql state +draw+) (format t "It's a draw!~%"))
        ((eql state +illegal-state+) (format t "Bugs and glitches!~%"))
        (t (throw 'non-board-state state)))) ;Shouldn't happen.

(defun get-human-move (board player)
  (loop (let ((input (pu:prompt-l
                      (format nil "Make your move (c r), ~A"
                              (char-upcase player)))))
          ;;Input in right format?
          (when (and (= 3 (length input))
                     (eql #\  (aref input 1)))
            ;;Input sane data, aka nums?
            (let* ((r (aref input 0))
                   (c (aref input 2))
                   (r (digit-char-p r))
                   (c (digit-char-p c))
                   ;;Input makes for a legal move?
                   (legal-moves (get-legal-moves board))
                   (move (list c r)))
              (when (and r
                         c
                         (< r (array-dimension board 0))
                         (< c (array-dimension board 1))
                         (member move legal-moves :test #'equal))
                (return-from get-human-move move)))))))

(defun wrap-ai-fn (ai-fn)
  (lambda (board player)
    (format t "~A calculates the next move...~%" (char-upcase player))
    (funcall ai-fn board player)))

(defun game-loop (x-fn o-fn)
  (let ((board (make-new-board))
        (player (aref #(#\x #\o) (random 2))))
    (loop for state = (get-game-state board)
          do (print-game-board board)
          until (not (eql state +in-play+))
          do (make-move board
                        (if (eql player #\x)
                            (funcall x-fn board player)
                            (funcall o-fn board player))
                        player)
          do (setf player (other-player player))
          finally (print-game-result state))))

(defun main ()
  (loop (format t "~{~A~%~}" (list "Select an option:"
                                   "1) 1-player"
                                   "2) 2-player"
                                   "3) Exit"))
        (let ((option (pu:prompt-i ">")))
          (case option
            (1
             (game-loop #'get-human-move (wrap-ai-fn (get-random-ai-fn))))
            (2
             (game-loop #'get-human-move #'get-human-move))
            (3
             (return-from main 'finished))))))

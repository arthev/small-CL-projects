(in-package :tic-tac-toe)

(defconstant +x-won+ 'x-won)
(defconstant +o-won+ 'o-won)
(defconstant +in-play+ 'in-play)
(defconstant +illegal-state+ 'illegal-state)
(defconstant +draw+ 'draw)

(defun make-new-board ()
  (make-array '(3 3) :initial-element nil))

(defun other-player (player)
  (if (eql #\x player)
      #\o
      #\x))

(defun get-legal-moves (board)
  (loop for row from 0 below (array-dimension board 0)
        append (loop for column from 0 below (array-dimension board 1)
                     when (null (aref board row column))
                       collect `(,row ,column))))

;;;Assume move is a list of two numbers
(defun make-move (board move player)
  (let ((legal-moves (get-legal-moves board)))
    (unless (member move legal-moves :test #'equal)
      (throw 'illegal-move move))
    (setf (aref board (car move) (cadr move)) player)
    board))
      
(defun undo-move (board move)
  (let ((legal-moves (get-legal-moves board)))
    (when (member move legal-moves :test #'equal)
      (throw 'illegal-remove move))
    (setf (aref board (car move) (cadr move)) nil)
    board))

(defun get-line-sums (board player)
  (append
   ;;check rows
   (loop for row from 0 below 3
         collect (loop for column from 0 below 3
                       when (eql (aref board row column) player)
                         count player))
   ;;check columns
   (loop for column from 0 below 3
         collect (loop for row from 0 below 3
                       when (eql (aref board row column) player)
                         count player))
   ;;check diagonals
   (list
    (loop for i from 0 to 2
          when (eql (aref board i i) player)
            count player)
    (loop for i from 0 to 2
          when (eql (aref board i (- 2 i)) player)
            count player))))

(defun win-p (board player)
  (let ((line-sums (get-line-sums board player)))
    (member 3 line-sums :test #'=)))

(defun get-game-state (board)
  (let ((x-win (win-p board #\x))
        (o-win (win-p board #\o))
        (legal-moves (get-legal-moves board)))
    (cond ((and x-win o-win) +illegal-state+)
          (x-win +x-won+)
          (o-win +o-won+)
          (legal-moves +in-play+)
          (t +draw+))))

(defun get-win-state (player)
  (if (eql #\x player) +x-won+ +o-won+))

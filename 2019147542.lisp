; # assn 1 : cellularautomata1d
(defun automation (left center right)
    (cond ((and (eql left 1) (eql center 1) (eql right 1)) 0)
        ((and (eql left 1) (eql center 1) (eql right 0)) 0)
        ((and (eql left 1) (eql center 0) (eql right 1)) 1)
        ((and (eql left 1) (eql center 0) (eql right 0)) 1)
        ((and (eql left 0) (eql center 1) (eql right 1)) 0)
        ((and (eql left 0) (eql center 1) (eql right 0)) 1)
        ((and (eql left 0) (eql center 0) (eql right 1)) 1)
        ((and (eql left 0) (eql center 0) (eql right 0)) 0)
    )
)

(defun cellularautomata1d (initialBoard time)
    (if (eq time 0)
        initialBoard
    ;else
        (let (resultBoard '())
            (dotimes (i (length initialBoard))
                (let* ((left (if (= i 0) 0 (nth (1- i) initialBoard)))
                    (center (nth i initialBoard))
                    (right (if (= i (1- (length initialBoard))) 0 (nth (1+ i) initialBoard)))
                    (result (automation left center right)))
                    (push result resultBoard)
                    )
            )
            (setq initialBoard (reverse resultBoard))
            ;recursive call
            (cellularAutomata1d initialBoard (1- time))
        )
        
    )
)

; # assn 2 : cellularautomata2d
(defun make2dlist (height width)
  (let ((result '()))
    (dotimes (i height)
      (let ((row '()))
        (dotimes (j width)
          (setq row (cons 0 row))) ; 각 열을 0으로 초기화
        (setq result (cons (reverse row) result)))) ; 행을 추가
    (reverse result))) ; 역순으로 결과 반환

(defun listtostring (list1)
    (cond ((null list1) "")
        ((listp (car list1))
            (concatenate 'string (listtostring(car list1)) (listtostring (cdr list1))) ;recursive call
        ) 
        (t (concatenate 'string (format nil "~a" (car list1)) (listtostring (cdr list1)))) ;recursive call when list1 is 1d list
    )
)

(defun formatlist (list1) ;format list to print, 개행 문자를 리스트마다 삽입, ~%를 포맷 형식에 넣어야 인식함
    (cond ((null list1) "")
        (t (concatenate 'string (listtostring (car list1)) (format nil "~%") (formatlist (cdr list1)))) ;recursive call: list가 끝날 때마다 개행 추가, ~%를 포맷 형식에 넣어야 인식함
    )
)   

(defun cellularAutomata2d (initialBoard time)
    (if (zerop time)
    ;~%~a라는 형식으로 뒤에 있는 변수 출력, ~%는 포맷이아니라 엔터를 맨 앞에 넣겠다는 의미(c의 %d%d와 같은 역할)
        (format nil "~%~a" (formatlist initialBoard))
    ;else
        ; 결과를 저장할 0으로 초기화된 2차원 배열 생성
        (let* ((resultboard (make2dlist (length initialboard) (length (nth 0 initialboard)))))
            ;2차원 배열의 모든 원소마다 주변의 8개 원소중 1인 원소의 갯수를 센다.
            (dotimes (i (length initialBoard))
                (dotimes (j (length (nth 0 initialBoard)))
                    (let* ((left (if (= j 0) 0 (nth (1- j) (nth i initialBoard))))
                        (center (nth j (nth i initialBoard)))
                        (right (if (= j (1- (length (nth i initialBoard)))) 0 (nth (1+ j) (nth i initialBoard))))
                        (up (if (= i 0) 0 (nth j (nth (1- i) initialBoard))))
                        (down (if (= i (1- (length initialBoard))) 0 (nth j (nth (1+ i) initialBoard))))
                        (upleft (if (or (= i 0) (= j 0)) 0 (nth (1- j) (nth (1- i) initialBoard))))
                        (upright (if (or (= i 0) (= j (1- (length (nth i initialBoard))))) 0 (nth (1+ j) (nth (1- i) initialBoard))))
                        (downleft (if (or (= i (1- (length initialBoard))) (= j 0)) 0 (nth (1- j) (nth (1+ i) initialBoard))))
                        (downright (if (or (= i (1- (length initialBoard))) (= j (1- (length (nth i initialBoard))))) 0 (nth (1+ j) (nth (1+ i) initialBoard))))
                        (count (+ left right up down upleft upright downleft downright))
                        (result (automation2d center count)))
                        (setf (nth j (nth i resultboard)) result)
                    )
                )
            )
            (setq initialboard resultboard)
            (cellularAutomata2d initialBoard (1- time))
        )
    )
)

(defun automation2d (cell count)
    (cond ((= count 2) cell)
          ((= count 3) 1)
          (t 0)
    )   
)
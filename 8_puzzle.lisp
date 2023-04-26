
(defstruct problem
  (initial-state (required)) ; A state in the domain
  (goal nil)                ; Optionally store the desired state here.
  (num-expanded 0)    ; Number of nodes expanded in search for solution.
  (iterative? nil)          ; Are we using an iterative algorithm?
  )

(defstruct (8puzzle-state (:conc-name nil) (:type list))
(low1 '(E 1 3)) (low2 '(4 2 5)) (low3 '(7 8 6)))

(defstruct (8puzzle-problem(:include problem (initial-state (make-8puzzle-state))))) 

;;; Transform state to a seq. For example, the state is ((E 1 3) (4 2 5) (7 8 6)),
;;; the output of this function will be (E 1 3 4 2 5 7 8 6). Using a sequence like 
;;; this will be easier for us to get the number os misplaced ties.
(defmethod state2seq (state)
(if  (NULL state) nil
 (concatenate 'list (low1 state) (low2 state) (low3 state))))
 
 
;;; Transform a seq to a state.
 (defmethod seq2state (seq)
 (if(not (NULL seq)) 
 (progn
 (setf low1 (list (elt seq 0) (elt seq 1)(elt seq 2)) )
 (setf low2 (list (elt seq 3) (elt seq 4)(elt seq 5)) )
 (setf low3 (list (elt seq 6) (elt seq 7)(elt seq 8)) )
 (list low1 low2 low3))
 nil))
 
;;; Test whether the state is a goal state, if so, return t.
(defmethod goal-test((problem problem) state)
(and (equal (low1 state) '(1 2 3)) (equal (low2 state) '(4 5 6)) (equal (low3 state) '(7 8 E))))


;;; Get the space E's place (0~8) in the sequence. For example, if 
;;; the sequence is (E 1 3 4 2 5 7 8 6), it will return 0.     
(defmethod get-space-place(seq)
 (cond ((equal (elt seq 0) 'E)  0)
       ((equal  (elt seq 1) 'E) 1)
       ((equal  (elt seq 2) 'E) 2)
       ((equal (elt seq 3) 'E)  3)
       ((equal  (elt seq 4) 'E) 4)
       ((equal  (elt seq 5) 'E) 5)
        ((equal (elt seq 6)'E)  6)
       ((equal  (elt seq 7) 'E) 7)
       ((equal  (elt seq 8) 'E) 8)      
       (t nil)))       
       
;;; There are four actions we may have to "move" the space E: 
;;; L(Left), R(Right), U(Up), D(Down). However, for a state/sequence,
;;; some actions are invalid. This function returns the valid 
;;; actions of the input sequence.       
(defmethod get-actions(seq)
 (cond ((= 0 (get-space-place  seq)) '(DOWN RIGHT))
       ((= 1 (get-space-place  seq)) '(DOWN LEFT RIGHT))
       ((= 2 (get-space-place  seq)) '(DOWN LEFT))
       ((= 3 (get-space-place  seq)) '(UP DOWN RIGHT))
       ((= 4 (get-space-place  seq)) '(UP DOWN LEFT RIGHT))
       ((= 5 (get-space-place  seq)) '(UP DOWN LEFT))
       ((= 6 (get-space-place  seq)) '(UP RIGHT))
       ((= 7 (get-space-place  seq)) '(UP LEFT RIGHT))
       ((= 8 (get-space-place  seq)) '(UP LEFT))
       (t nil))
) 

;;; Given the action and seq, we will get a new sequence.
;;; This function returns the new sequence.
(defmethod next-seq(action seq)
   (setf space (get-space-place seq))
   (let ((nextseq seq))
   (cond((equal action 'LEFT) (setf nextspace(- space 1)) (setf (elt nextseq space) (elt nextseq nextspace)) (setf(elt nextseq nextspace) 'E) nextseq)
       ((equal action 'RIGHT) (setf nextspace(+ space 1)) (setf (elt nextseq space) (elt nextseq nextspace)) (setf(elt nextseq nextspace) 'E) nextseq)
       ((equal action 'UP) (setf nextspace(- space 3)) (setf (elt nextseq space) (elt nextseq nextspace)) (setf(elt nextseq nextspace) 'E) nextseq)
       ((equal action 'DOWN) (setf nextspace(+ space 3)) (setf (elt nextseq space) (elt nextseq nextspace)) (setf(elt nextseq nextspace) 'E) nextseq)
       (t nil))))

;;; This function returns the next state, given the previous state and action.	   
(defmethod next-state(action state)
   (setf seq (state2seq state))
  (seq2state(next-seq action seq)))
  
 ;;; Return pairs of action and new state as successors of the input state  
(defmethod successors ((problem problem) state)
  (let ((pairs nil))
  (loop for action in (get-actions (state2seq state)) 
  do(let ((new-state (next-state action state )))
   (when new-state
   (push (cons action new-state) pairs))))
   pairs))


;;; Heuristic function, use the number of misplaced ties.     
(defmethod h-cost ((problem problem) state) 
  (get-misplaced-numbers state)
  )
 
;;; Get the number of misplaced ties in a given state
(defmethod get-misplaced-numbers (state)
   (setf seq (state2seq state))
   (setf goal-seq '(1 2 3 4 5 6 7 8 E))
   (setf count 9)
   (loop for tie in '(0 1 2 3 4 5 6 7 8) do
   (if (equal (elt seq tie) (elt goal-seq tie)) (decf count))) 
    count)

;;; Edge-cost set to 1.
(defmethod edge-cost ((problem problem) node action state)
  1)

;;;;;; node structure, contains state, g-cots, h-cost, f-cost, parent(convinience to print the path),
;;;;;  successors, expanded?(for search optimization)
(defstruct node
  (state (required))        ; a state in the domain
  (parent nil)              ; the parent node of this node
  (action nil)              ; the domain action leading to state
  (successors nil)          ; list of sucessor nodes
  (unexpanded nil)          ; successors not yet examined (SMA* only)
  (depth 0)                 ; depth of node in tree (root = 0)
  (g-cost 0)                ; path cost from root to node
  (h-cost 0)                ; estimated distance from state to goal
  (f-cost 0)                ; g-cost + h-cost
  (expanded? nil)           ; any successors examined?
  )
  
;;; Function to expand the node, returns a list containing successor nodes.
(defun expand (node problem)
  (unless (and (node-expanded? node) (not (problem-iterative? problem)))
    (setf (node-expanded? node) t)
    (incf (problem-num-expanded problem))
    (display-expand problem node)
    (let ((nodes nil))
         (loop for (action . state) in (successors problem (node-state node)) do
	   (let ((g (+ (node-g-cost node) (edge-cost problem node action state)))(h (h-cost problem state)))
	     (push(make-node :parent node :action action :state state
	       :depth (1+ (node-depth node)) :g-cost g :h-cost h
	       :f-cost (max (node-f-cost node) (+ g h)))
	      nodes)))
     nodes)))



;;;;;;;;;;;;;; create the start node with the initial state
(defun create-start-node (problem)
  "Make the starting node, corresponding to the problem's initial state."
  (let ((h (h-cost problem (problem-initial-state problem))))
    (make-node :state (problem-initial-state problem)
	       :h-cost h :f-cost h)))

		   
;;;; Search Algorithms That Use Heuristic Information
(defun a*-search (problem)
  (let ((nodes (make-initial-queue problem))
	node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (if (goal-test problem (node-state node)) (RETURN node))
	  (enqueue-by-fcost nodes (expand node problem) ))))
	  
;;;; initiate an empty queue, enqueue the start node
(defun make-initial-queue (problem)
  (let ((q (make-empty-queue)))
    (enqueue-by-fcost q (list (create-start-node problem)))
    q))
;;;;;;;;;;define a queue structure and some operations on queue
(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-q))

;;;;;check whether the queue is empty
(defun empty-queue? (q)
  (= (length (q-elements q)) 0))
	   
;;;;Insert the items according to the fcost.	 
(defun enqueue-by-fcost (q items)
  (setf (q-key q) #'node-f-cost)
  (when (null (q-elements q))
    (setf (q-elements q) (make-array 100 :fill-pointer 0 :adjustable t)))
  (loop for item in items do
       (insert (q-elements q) item #'node-f-cost)))  
 

;;;insert one item by the key value(fcost) 
(defun insert(qelements item key)
(vector-push-extend item qelements)
(setf count 0)
(setf num (fill-pointer qelements))
(loop for i from 0 to (- num 2)do
  (if (>= (funcall key (aref qelements i)) (funcall key item)) (incf count)))
(loop for i from (- num 2) downto count do
(setf (aref qelements (+ i 1)) (aref qelements i)))
(setf (aref qelements count) item)
)


;;;  Return the element at the front of the queue.	   
(defun queue-front (q)
  (elt (q-elements q) 0))
  
;;;;  Remove the element from the front of the queue and return it.
(defun remove-front (q)
(vector-pop (q-elements q)))
"Return a list of actions that will lead to the node's state."	   	   
(defun solution-actions (node &optional (actions-so-far nil))
  (cond ((null node) actions-so-far)
	((null (node-parent node)) actions-so-far)
	(t (solution-actions (node-parent node)
			     (cons (node-action node) actions-so-far)))))
				 
"Return a list of the nodes along the path to the solution."
(defun solution-nodes (node &optional (nodes-so-far nil))
  (cond ((null node) nodes-so-far)
	(t (solution-nodes (node-parent node)
			   (cons node nodes-so-far)))))
;;;;;;;;;;print the number of actions taken to reach the goal node

	
;;;;;;;;;Use a search algorithm (a*-search by default) to search the goal,print a list of actions leading to the goal.
(defun solve (problem &optional (algorithm 'a*-search))
  "Print a list of actions that will solve the problem (if possible).
  Return the node that solves the problem, or nil."
  (setf (problem-num-expanded problem) 0)
  (let ((node (funcall algorithm problem)))
    (print-solution problem node)
    node))
;;;;;;;;;;;formatted print the solution, one column is action, the other one is state
(defun print-solution (problem node)
  "Print a table of the actions and states leading up to a solution."
  (loop for n in (solution-nodes node) do
       (format t "~A~%"
	       (node-state n)))
  (format t "Total of ~D node~:P expanded."
	  (problem-num-expanded problem))
  node)

;;;;;;;;;;;;;;;;display message when expanding node, can't get through compilation with the DPRINT in original code.
(defmethod display-expand ((problem problem) node)
   )

;;;CALLING
(setq p1 (make-8puzzle-problem :initial-state (make-8puzzle-state :low1 '(E 1 3) :low2 '(4 2 5) :low3 '(7 8 6))))


(solve p1 'a*-search)




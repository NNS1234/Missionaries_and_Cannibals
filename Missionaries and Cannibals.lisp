; your code goes here


;;;; The Missionaries and Cannibals Domain from aima

 "The problem is to move M missionaries and C cannibals from one side
 of a river to another, using boat B that holds at most 6 people one time,
 in such a way that the cannibals never outnumber the missionaries in
 any one place. "
;;;; Defining Problems

(defstruct problem
  "A problem is defined by the initial state, and the type of problem it is."
  (initial-state (required)) ; A state in the domain
  (goal nil)                 ; Optionally store the desired state here.
  (num-expanded 0)           ; Number of nodes expanded in search for solution.
  (iterative? nil)           ; Are we using an iterative algorithm?
  )
;;;;;;;;;;;define the cannibal-problem which interitates the problem structure
(defstruct (cannibal-problem 
	       (:include problem (initial-state (make-cannibal-state)))) 
)

;;;;;;;;;;test whether it reaches the goal that all the m and c are transfered to right bank
(defmethod goal-test ((problem cannibal-problem) state)
  "The goal is to have no missionaries or cannibals left on the first side."
  (= 0 (m1 state) (c1 state)))
;;;;;;;;;;;;;;;;;heuristic cost, can be used for A* search 
;;; When we define a new subtype of problem, we need to define a SUCCESSORS
;;; method. We may need to define methods for GOAL-TEST, H-COST, and
;;; EDGE-COST, but they have default methods which may be approp
(defmethod h-cost ((problem problem) state) 
  ( / (+ (m1 state)(c1 state))2) )
  ;( / (+ (m1 state)(c1 state))3) )
  ;;( / (+ (m1 state)(c1 state))4) )
  ;;( / (+ (m1 state)(c1 state))5) )
;;;;;;;;;;;;;;;;;edge cost, 1 by default
(defmethod edge-cost ((problem problem) node action state)
 1)
;;;;;;;;;;;;;;;;return pairs of action and state as successors 
(defmethod successors ((problem cannibal-problem) state)
  "Return a list of (action . state) pairs.  An action is a triple of the
  form (delta-m delta-c delta-b), where a positive delta means to move from
  side 1 to side 2; negative is the opposite.  For example, the action (1 0 1)
  means move one missionary and 1 boat from side 1 to side 2."
  (let ((pairs nil))
   (loop for action in '(   
 (+1 +0 +1) (+0 +1 +1) (+1 +2 +1)(+2 +3 +1)
 (+2 +0 +1) (+0 +2 +1) (+1 +1 +1)(+2 +4 +1)
 (+3 +0 +1) (+2 +1 +1) (+0 +3 +1)(+1 +5 +1)
 (+4 +0 +1) (+3 +1 +1) (+2 +2 +1) (+0 +4 +1)(+1 +4 +1)
 (+5 +0 +1) (+4 +1 +1) (+3 +2 +1) (+0 +5 +1) (+1 +3 +1)
 (+6 +0 +1) (+5 +1 +1) (+4 +2 +1) (+3 +3 +1) (+0 +6 +1)
 (-1 +0 -1) (+0 -1 -1) (-1 -2 -1) (-2 -3 -1)
 (-2 -0 -1) (-0 -2 -1) (-1 -1 -1)(-2 -4 -1)
 (-3 -0 -1) (-2 -1 -1) (-0 -3 -1)(-1 -5 -1)
 (-4 -0 -1) (-3 -1 -1) (-2 -2 -1) (-0 -4 -1)(-1 -4 -1)
 (-5 +0 -1) (-4 -1 -1) (-3 -2 -1) (-0 -5 -1) (-1 -3 -1)
 (-6 -0 -1) (-5 -1 -1) (-4 -2 -1) (-3 -3 -1) (-0 -6 -1)
						) do
	(let ((new-state (take-the-boat state action)))
	  (when (and new-state (not (cannibals-can-eat? new-state)))
	    (push (cons action new-state) pairs))))
   pairs))
   
;;;;;;;;;;;;define the cannibal state 
(defstruct (cannibal-state (:conc-name nil) (:type list))
  "The state says how many missionaries, cannibals, and boats on each
  side.  The components m1,c1,b1 stand for the number of missionaries,
  cannibals and boats, respectively, on the first side of the river.
  The components m2,c2,b2 are for the other side of the river."
  ;; We need to represent both sides (rather than just one as on [p 68])
  ;; because we have generalized from 3+3 people to M+C.  Incidently, we
  ;; also generalized from 1 boat to B boats.
  (m1 3) (c1 3) (b1 1) (m2 0) (c2 0) (b2 0))
  
;;;;;;;;;;;;;;;;actions and calculate

(defun take-the-boat (state action)
  "Move a certain number of missionaries, cannibals, and boats (if possible)."
  (destructuring-bind (delta-m delta-c delta-b) action
    (if (or (and (= delta-b +1) (> (b1 state) 0))
	    (and (= delta-b -1) (> (b2 state) 0)))
	(let ((new (copy-cannibal-state state)))
	  (decf (m1 new) delta-m) (incf (m2 new) delta-m)
	  (decf (c1 new) delta-c) (incf (c2 new) delta-c)
	  (decf (b1 new) delta-b) (incf (b2 new) delta-b)
	  (if (and (>= (m1 new) 0) (>= (m2 new) 0)
		   (>= (c1 new) 0) (>= (c2 new) 0))
	      new
	    nil))
      nil)))
;;;;;;;;;;;;;;;;; whether it is a valid state, 
(defun cannibals-can-eat? (state)
  (or (> (c1 state) (m1 state) 0)
      (> (c2 state) (m2 state) 0)))

	  

(defstruct node
  "Node for generic search.  A node contains a state, a domain-specific
  representation of a point in the search space.  A node also contains 
  bookkeeping information such as the cost so far (g-cost) and estimated cost 
  to go (h-cost)."
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
  (completed? nil)          ; all successors examined? (SMA* only)
  )

(defun expand (node problem)
  "Generate a list of all the nodes that can be reached from a node."
  ;; Note the problem's successor-fn returns a list of (action . state) pairs.
  ;; This function turns each of these into a node.
  ;; If a node has already been expanded for some reason, then return no nodes,
  ;; unless we are using an iterative algorithm.
  (unless (and (node-expanded? node) (not (problem-iterative? problem)))
    (setf (node-expanded? node) t)
    (incf (problem-num-expanded problem))
    (display-expand problem node)
    (let ((nodes nil))
      (loop for (action . state) in (successors problem (node-state node)) do
	   (let* ((g (+ (node-g-cost node) 
			(edge-cost problem node action state)))
		  (h (h-cost problem state)))
	     (push
	      (make-node 
	       :parent node :action action :state state
	       :depth (1+ (node-depth node)) :g-cost g :h-cost h
	       ;; use the pathmax equation [p 98] for f:
	       :f-cost (max (node-f-cost node) (+ g h)))
	      nodes)))
      nodes)))

(defun create-start-node (problem)
  "Make the starting node, corresponding to the problem's initial state."
  (let ((h (h-cost problem (problem-initial-state problem))))
    (make-node :state (problem-initial-state problem)
	       :h-cost h :f-cost h)))

;;;; Simple Search Algorithms

;;; Here we define the GENERAL-SEARCH function, and then a set of
;;; search functions that follow specific search strategies.  None of
;;; these algorithms worries about repeated states in the search.
;;; Here we define the GENERAL-SEARCH function, and then a set of
;;; search functions that follow specific search strategies.  None of
;;; these algorithms worries about repeated states in the search.
(defun general-search (problem queuing-fn)
  "Expand nodes according to the specification of PROBLEM until we find
  a solution or run out of nodes to expand.  The QUEUING-FN decides which
  nodes to look at first."
  (let ((nodes (make-initial-queue problem queuing-fn))
	node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (if (goal-test problem (node-state node)) (RETURN node))
	  (funcall queuing-fn nodes (expand node problem)))))


;;;; Utility Function

(defun make-initial-queue (problem queuing-fn)
  (let ((q (make-empty-queue)))
    (funcall queuing-fn q (list (create-start-node problem)))
    q))
;;; In this file we show algorithms that worry about repeated states.
;;; Here are the three ways to deal with repeated states, from [p 82]:
;;;; Search Algorithms That Avoid Repeated States


(defun A*-search (problem)
  "Search the nodes with the best f cost first.  If a node is ever reached by
  two different paths, keep only the better path."
  (general-search problem (make-eliminating-queuing-fn #'node-f-cost)))

(defun make-eliminating-queuing-fn (eval-fn)
  (let ((table (make-hash-table :test #'equal)))
    #'(lambda (old-q nodes)
	(enqueue-by-priority
	 old-q
	 (let ((result nil))
	  (loop for node in nodes do
	       (let ((old-node (gethash (node-state node) table)))
		 (cond
		  ((null old-node)
		   ;; First time we've reached state; just return node
		   (setf (gethash (node-state node) table) node)
		   (push node result))
		  ((<= (funcall eval-fn old-node) (funcall eval-fn node))
		   ;; If the old node is better, discard the new node
		   nil)
		  (t;; Otherwise, discard the old node
		   (setf (node-expanded? old-node) t)
		   (setf (gethash (node-state node) table) node)
		   (push node result)))))
	  (nreverse result))
	 eval-fn))))
;;;; The Queue datatype

;;; We can remove elements form the front of a queue.  We can add elements in
;;; three ways: to the front, to the back, or ordered by some numeric score.
;;; This is done with the following enqueing functions, which make use of the
;;; following implementations of the elements:
;;;   ENQUEUE-AT-FRONT - elements are a list
;;;   ENQUEUE-AT-END   - elements are a list, with a pointer to end
;;;   ENQUEUE-BY-PRIORITY - elements are a heap, implemented as an array
;;; The best element in the queue is always in position 0.

;;; The heap implementation is taken from "Introduction to Algorithms" by
;;; Cormen, Lieserson & Rivest [CL&R], Chapter 7.  We could certainly speed
;;; up the constant factors of this implementation.  It is meant to be clear
;;; and simple and O(log n), but not super efficient.  Consider a Fibonacci
;;; heap [Page 420 CL&R] if you really have large queues to deal with.		   	   

(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

;;;; Basic Operations on Queues

(defun make-empty-queue () (make-q))

(defun empty-queue? (q)
  "Are there no elements in the queue?"
  (= (length (q-elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (q-elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (if (listp (q-elements q))
      (pop (q-elements q))
    (heap-extract-min (q-elements q) (q-key q))))


(defun enqueue-by-priority (q items key)
  "Insert the items by priority according to the key function."
  ;; First make sure the queue is in a consistent state
  (setf (q-key q) key)
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
  (loop for item in items do
       (heap-insert (q-elements q) item key)))

;;;; The Heap Implementation of Priority Queues
(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be 
  larger than its children.  If it is, move heap[i] down where it belongs.
  [Page 143 CL&R]."
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (- (length heap) 1))
	smallest)
    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
					 (heap-val heap i key)))
		       l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
	(setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

(defun heap-extract-min (heap key)
  "Pop the best (lowest valued) item off the heap. [Page 150 CL&R]."
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Put an item into a heap. [Page 150 CL&R]."
  ;; Note that ITEM is the value to be inserted, and KEY is a function
  ;; that extracts the numeric value from the item.
  (vector-push-extend nil heap)
  (let ((i (- (length heap) 1))
	(val (funcall key item)))
    (loop while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val))
      do (setf (aref heap i) (aref heap (heap-parent i))
	       i (heap-parent i)))
    (setf (aref heap i) item)))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

(defun heap-sort (numbers &key (key #'identity))
  "Return a sorted list, with elements that are < according to key first."
  ;; Mostly for testing the heap implementation
  ;; There are more efficient ways of sorting (even of heap-sorting)
  (let ((heap (make-heap))
	(result nil))
    (loop for n in numbers do (heap-insert heap n key))
    (while (> (length heap) 0) do (push (heap-extract-min heap key) result))
    (nreverse result)))
		   
		   

;;; Solutions are represented just by the node at the end of the path.  The
;;; function SOLUTION-ACTIONS returns a list of actions that get there.  It
;;; would be problematic to represent solutions directly by this list of
;;; actions, because then we couldn't tell a solution with no actions from a
;;; failure to find a solution. 

(defun solution-actions (node &optional (actions-so-far nil))
  "Return a list of actions that will lead to the node's state."
  (cond ((null node) actions-so-far)
	((null (node-parent node)) actions-so-far)
	(t (solution-actions (node-parent node)
			     (cons (node-action node) actions-so-far)))))

(defun solution-nodes (node &optional (nodes-so-far nil))
  "Return a list of the nodes along the path to the solution."
  (cond ((null node) nodes-so-far)
	(t (solution-nodes (node-parent node)
			   (cons node nodes-so-far)))))

(defun solve (problem &optional (algorithm 'A*-search))
  "Print a list of actions that will solve the problem (if possible).
  Return the node that solves the problem, or nil."
  (setf (problem-num-expanded problem) 0)
  (let ((node (funcall algorithm problem)))
    (print-solution problem node)
    node))

(defun print-solution (problem node)
  "Print a table of the actions and states leading up to a solution."
  (if node
      (format t "~&Action ~20t--> State~%===============================~%")
    (format t "~&No solution found.~&"))
  (loop for n in (solution-nodes node) do
       (format t "~&~A ~20t--> ~A~%"
	       (or (node-action n) "") (node-state n)))
  (format t "=======================================~%Total of ~D node~:P expanded."
	  (problem-num-expanded problem))
  node)

;;;;;;;;;;;;;print

(defmethod print-structure ((node node) stream) 
  (format stream "#<NODE f(~D) = g(~D) + h(~D) state:~A> " (node-f-cost node)
	  (node-g-cost node) (node-h-cost node) (node-state node)))

;;;;;;;;;;;;;;;display message when expanding node, can't get through compilation with the DPRINT in original code.	  
(defmethod display-expand ((problem problem) node)
   )


;calling function
(setq p1 (make-cannibal-problem :initial-state (make-cannibal-state :m1 15 :c1 15 :b1 1)))
(setq p2 (make-cannibal-problem :initial-state (make-cannibal-state :m1 20 :c1 20 :b1 1)))

(solve p1 'A*-search)
(solve p2 'A*-search)
 
	 
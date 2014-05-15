(in-package :famiclom)

;;;; References:
;; http://nesdev.com/NESDoc.pdf Chapter 5
;; http://www.gamefaqs.com/nes/916386-nes/faqs/2948
;; http://wiki.nesdev.com/w/index.php/Input_devices
;; http://wiki.nesdev.com/w/index.php/Standard_controller

(defvar *keymap*
  '((:a      . :sdl-key-z)
    (:b      . :sdl-key-x)
    (:select . :sdl-key-space)
    (:start  . :sdl-key-return)
    (:up     . :sdl-key-up)
    (:down   . :sdl-key-down)
    (:left   . :sdl-key-left)
    (:right  . :sdl-key-right)))

(deftype bool () '(unsigned-byte 1))

(defstruct pad
  (buttons (make-array 8 :element-type 'bool))
  (strobe '#0=(:a :b :select :start :up :down :left :right . #0#)))

(defvar *pad* (make-pad) "An input device to retrieve commands from.")

(defun %pad-index (pad)
  (let ((key (first (pad-strobe pad))))
    (position key *keymap* :key #'first)))

(defun get-state (pad)
  "Get the state of the button currently strobed by PAD."
  (aref (pad-buttons pad) (%pad-index pad)))

(defun next-state (pad)
  "Update the strobe value of PAD."
  (pop (pad-strobe pad)))

(defun reset-strobe (pad)
  "Reset PAD's strobe starting at :a."
  (with-accessors ((strobe pad-strobe)) pad
    (loop :until (eql :a (first strobe)) :do (pop strobe))))

(defun get-byte-input% (addr)
  (declare (ignore addr))
  (prog1 (get-state *pad*)
    (next-state *pad*)))

(let ((last-input 0))
  (defun (setf get-byte-input%) (new-val addr)
    (declare (ignore addr))
    (prog1
        (when (and (eql 0 new-val)
                   (eql 1 last-input))
          (reset-strobe *pad*))
      (setf last-input new-val))))

(defun set-key (target-key pad value)
  (loop
     :for (button . key) :in *keymap*
     :for index :from 0
     :do
     (when (eq key target-key)
       (setf (aref (pad-buttons pad) index) value))))

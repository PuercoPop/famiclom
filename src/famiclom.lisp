(in-package :famiclom)

(defstruct nes
  "A \"headless\" NES."
  (cpu    (make-cpu))
  (ram    (make-array #x0800 :element-type 'u8))
  (ppu    (make-ppu))
  (apu    (make-apu))
  (mapper nil)
  (rom    nil))

(defvar *nes* (make-nes))

(defun get-byte-ram% (addr)
  (aref (nes-ram *nes*) (logand addr #x7ff)))

(defun (setf get-byte-ram%) (new-val addr)
  (setf (aref (nes-ram *nes*) (logand addr #x7ff)) new-val))

(defun get-byte (addr)
  (cond ((< addr #x2000) (get-byte-ram% addr))
        ((< addr #x4000) (get-byte-ppu% addr))
        ((< addr #x4018) (get-byte-input% addr))
        (t (get-mapper (nes-mapper *nes*) addr))))

(defun (setf get-byte) (new-val addr)
  (cond ((< addr #x2000) (setf (get-byte-ram% addr) new-val))
        ((< addr #x4000) (setf (get-byte-ppu% addr) new-val))
        ((< addr #x4018) (setf (get-byte-input% addr) new-val))
        (t (set-mapper (nes-mapper *nes*) addr new-val))))

(defun load-rom (file)
  "Load the given FILE into the NES."
  (let* ((rom (romreader:load-rom file))
         (mapper (getf (rom-metadata rom) :mapper)))
    (setf (nes-rom *nes*) rom)
    (setf (nes-mapper *nes*) (make-mapper (car mapper)))))

;; init for the nes is basically: load rom, absolute jmp to the contents of $FFFC

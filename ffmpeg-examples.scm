(use posix)
(use imlib2 ffmpeg traversal)

(define video "/home/andrei/video-datasets/sentences/adverb1-The_person_picked+up_the_giraffe_quickly.mov")
(video-length video)

(ffmpeg-open-video video)

(define frames (map-imlib-frame-from-video-indexed 
           (lambda (frame-nr index imlib)
            imlib)
           video))

(for-each-imlib-frame-from-video-indexed 
 (lambda (frame-nr index imlib)
  (image-save imlib (format #f "/tmp/frame-~a.png" index)))
 video)

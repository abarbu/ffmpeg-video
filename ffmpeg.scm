(module ffmpeg *
(import chicken scheme foreign traversal linear-algebra
        image-processing scheme2c-compatibility imlib2 foreigners
        srfi-1 posix extras)

(define (byte-ref p offset)
 (foreign-lambda* unsigned-char
                  (((c-pointer "char") p) (int offset))
                  "C_return(*(p+offset));"))

(foreign-declare "#include <libavcodec/avcodec.h>")
(foreign-declare "#include <libavformat/avformat.h>")
(foreign-declare "#include <libswscale/swscale.h>")
(foreign-declare "#include <Imlib2.h>")

(foreign-declare "
struct ffmpeg_video_t {
  AVFormatContext *pFormatCtx;
  int videoStream;
  AVCodecContext *pCodecCtx;
  AVFrame *pFrame;
  AVFrame *pFrameBGRA;
  uint8_t *buffer;
  struct SwsContext *img_convert_ctx;
  AVPacket packet;
  int frame;
  int videoFinished;
};")

(define-record ffmpeg ptr)
(define-foreign-type ffmpeg (c-pointer "struct ffmpeg_video_t"))

(define (ffmpeg-error loc msg . args)
 (abort (make-composite-condition
         (make-property-condition 'exn
                                  'location loc
                                  'message msg
                                  'arguments args)
         (make-property-condition 'ffmpeg))))

;; TODO Change these to raise exceptions
(foreign-declare #<<EOF
int ffmpeg_first_video_stream(struct ffmpeg_video_t *video) {
  if(av_find_stream_info(video->pFormatCtx)<0) {
    fprintf(stderr, "error: Can't get video stream information\n");
    exit(-1);
  }
  for(int i=0; i < video->pFormatCtx->nb_streams; i++)
    if(video->pFormatCtx->streams[i]->codec->codec_type==AVMEDIA_TYPE_VIDEO)
      return i;
  fprintf(stderr, "error: Can't find first video stream");
  exit(-1);
}

AVCodecContext *ffmpeg_get_codec(struct ffmpeg_video_t *video) {
  AVCodecContext *pCodecCtx = video->pFormatCtx->streams[video->videoStream]->codec;
  AVCodec *pCodec = avcodec_find_decoder(pCodecCtx->codec_id);
  if(pCodec==NULL) {
    fprintf(stderr,"error: Unsupported codec!");
    exit(-1);
  }
  if(avcodec_open(pCodecCtx, pCodec)<0) {
    fprintf(stderr,"error: Can't open codec!");
    exit(-1);
  }
  return pCodecCtx;
}

uint8_t *ffmpeg_get_frame(struct ffmpeg_video_t *video) {
  uint8_t *data = malloc(avpicture_get_size(PIX_FMT_BGRA,
					    video->pCodecCtx->width,
					    video->pCodecCtx->height) * sizeof(uint8_t));
  sws_scale(video->img_convert_ctx, (const uint8_t * const*)video->pFrame->data,
	    video->pFrame->linesize, 0,
	    video->pCodecCtx->height,
	    video->pFrameBGRA->data, video->pFrameBGRA->linesize);
  memcpy(data, video->buffer,
	 avpicture_get_size(PIX_FMT_BGRA,
			    video->pCodecCtx->width,
			    video->pCodecCtx->height) * sizeof(uint8_t));
  return data;
}

Imlib_Image *ffmpeg_get_frame_as_imlib(struct ffmpeg_video_t *video) {
  sws_scale(video->img_convert_ctx, (const uint8_t * const*)video->pFrame->data,
	    video->pFrame->linesize, 0,
	    video->pCodecCtx->height,
	    video->pFrameBGRA->data, video->pFrameBGRA->linesize);
  Imlib_Image *image =
    imlib_create_image_using_copied_data(video->pCodecCtx->width,
					 video->pCodecCtx->height,
					 (uint32_t*)video->buffer);
  return image;
}

int ffmpeg_next_frame(struct ffmpeg_video_t *video) {
  av_free_packet(&video->packet);
  int frameFinished;
  int nextFrameValid = av_read_frame(video->pFormatCtx, &video->packet) >= 0;
  if(nextFrameValid && video->packet.stream_index==video->videoStream) {
    avcodec_decode_video2(video->pCodecCtx, video->pFrame, &frameFinished, &video->packet);
    if(frameFinished) video->frame++;
    else ffmpeg_next_frame(video);
  } else if(nextFrameValid) {
    ffmpeg_next_frame(video);
  } else if(!video->videoFinished && !nextFrameValid) {
    // This is required because ffmpeg hangs on to many frames internally
    AVPacket packet;
    packet.data = 0;
    packet.size = 0;
    avcodec_decode_video2(video->pCodecCtx, video->pFrame, &frameFinished, &packet);
    if(frameFinished)
      video->frame++;
    else
      video->videoFinished = 1;
  }
  return !video->videoFinished;
}

struct ffmpeg_video_t *ffmpeg_open_video(char* filename) {
  struct ffmpeg_video_t *video = malloc(sizeof(struct ffmpeg_video_t));
  bzero(video, sizeof(struct ffmpeg_video_t));
  av_register_all();
  if(avformat_open_input(&video->pFormatCtx, filename, NULL, NULL)!=0) {
    fprintf(stderr, "error: Can't open video\n");
    free(video);
    return NULL;
  }
  video->videoStream = ffmpeg_first_video_stream(video);
  video->pCodecCtx = ffmpeg_get_codec(video);
  video->pFrame = avcodec_alloc_frame();
  video->pFrameBGRA = avcodec_alloc_frame();
  if(!video->pFrameBGRA || !video->pFrame) {
    fprintf(stderr,"error: Can't allocate frame!");
    avcodec_close(video->pCodecCtx);
    free(video);
    return NULL;
  }
  video->buffer =
    (uint8_t *)av_malloc(avpicture_get_size(PIX_FMT_BGRA,
					    video->pCodecCtx->width,
					    video->pCodecCtx->height) *
			 sizeof(uint8_t));
  avpicture_fill((AVPicture *)video->pFrameBGRA, video->buffer, PIX_FMT_BGRA,
		 video->pCodecCtx->width, video->pCodecCtx->height);
  video->img_convert_ctx =
    sws_getContext(video->pCodecCtx->width, video->pCodecCtx->height,
		   video->pCodecCtx->pix_fmt,
		   video->pCodecCtx->width, video->pCodecCtx->height,
		   PIX_FMT_BGRA, SWS_BICUBIC,
		   NULL, NULL, NULL);
  video->videoFinished = 0;
  video->frame = 0;
  av_init_packet(&video->packet);
  ffmpeg_next_frame(video);
  return video;
}
EOF
)

(define (ffmpeg-open-video filename)
 (let* ((video 
         (set-finalizer! (foreign-value "malloc(sizeof(struct ffmpeg_video_t))" ffmpeg)
                         ffmpeg-close-video))
        (r ((foreign-lambda* 
             int
             ((c-string filename)
              (ffmpeg video))
             "
  bzero(video, sizeof(video));
  av_register_all();
  if(avformat_open_input(&video->pFormatCtx, filename, NULL, NULL)!=0)
    C_return(1);
  video->videoStream = ffmpeg_first_video_stream(video);
  video->pCodecCtx = ffmpeg_get_codec(video);
  video->pFrame = avcodec_alloc_frame();
  video->pFrameBGRA = avcodec_alloc_frame();
  if(!video->pFrameBGRA || !video->pFrame) {
    avcodec_close(video->pCodecCtx);
    C_return(2);
  }
  video->buffer =
    (uint8_t *)av_malloc(avpicture_get_size(PIX_FMT_BGRA,
					    video->pCodecCtx->width,
					    video->pCodecCtx->height) *
			 sizeof(uint8_t));
  avpicture_fill((AVPicture *)video->pFrameBGRA, video->buffer, PIX_FMT_BGRA,
		 video->pCodecCtx->width, video->pCodecCtx->height);
  video->img_convert_ctx =
    sws_getContext(video->pCodecCtx->width, video->pCodecCtx->height,
		   video->pCodecCtx->pix_fmt,
		   video->pCodecCtx->width, video->pCodecCtx->height,
		   PIX_FMT_BGRA, SWS_BICUBIC,
		   NULL, NULL, NULL);
  video->videoFinished = 0;
  video->frame = 0;
  av_init_packet(&video->packet);
  ffmpeg_next_frame(video);
  C_return(0);") filename video)))
  (cond ((= r 0) video)
        ((= r 1) (ffmpeg-error 'ffmpeg-open-video "Can't find video" filename))
        ((= r 2) (ffmpeg-error 'ffmpeg-open-video "Can't allocate a frame" filename))
        (else (ffmpeg-error 'ffmpeg-open-video "Unknown error (bug)" filename)))))

(define ffmpeg-close-video
 (foreign-lambda*
  void
  ((ffmpeg video))
  "av_free(video->buffer);
   av_free(video->pFrameBGRA);
   av_free(video->pFrame);
   avcodec_close(video->pCodecCtx);
   av_close_input_file(video->pFormatCtx);
   av_free_packet(&video->packet);
   video->videoFinished = 1;
   free(video);"))

(define ffmpeg-video-finished?
 (foreign-lambda* bool ((ffmpeg video)) "C_return(video->videoFinished);"))

(define (ffmpeg-next-frame! video)
 ((foreign-lambda*
   bool
   ((ffmpeg video))
   "C_return(ffmpeg_next_frame(video));")
  video))

(define (ffmpeg-video-frame-index video)
 (foreign-lambda* int ((ffmpeg video)) "C_return(video->frame);"))

(define ffmpeg-video-width (foreign-lambda* int ((ffmpeg video)) "C_return(video->pCodecCtx->width);"))
(define ffmpeg-video-height (foreign-lambda* int ((ffmpeg video)) "C_return(video->pCodecCtx->height);"))
(define ffmpeg-video-frame-rate 
 (foreign-lambda*
  double ((ffmpeg video))
  "C_return((double)(video->pFormatCtx->streams[video->videoStream]->r_frame_rate.num)
            / (double)(video->pFormatCtx->streams[video->videoStream]->r_frame_rate.den));"))

(define (ffmpeg-video-frame-data video)
 (let* ((data ((c-function c-pointer ("ffmpeg_get_frame" ffmpeg)) video))
	(width (ffmpeg-video-width video))
	(height (ffmpeg-video-height video))
	(ppm (ppm-constant width height 0 0 0))
	(red (ppm-red ppm))
	(green (ppm-green ppm))
	(blue (ppm-blue ppm))
	(char-ref (c-sized-int-ptr-ref 1 #f)))
  (for-each-n
    (lambda (h)
     (for-each-n
       (lambda (w)
        (let ((index (* (+ (* h width) w) 4)))
         (matrix-set! red h w (char-ref data (+ index 2)))
         (matrix-set! green h w (char-ref data (+ index 1)))
         (matrix-set! blue h w (char-ref data index))))
      width))
   height)
  ((foreign-lambda void free c-pointer) data)
  ppm))

(define (ffmpeg-video-frame-data-as-imlib video)
 (make-image 
  ((foreign-lambda* c-pointer ((ffmpeg video)) 
                    "C_return(ffmpeg_get_frame_as_imlib(video));")
   video)))

(define (with-ffmpeg-video video-path f)
 (f (ffmpeg-open-video video-path)))

(define (compute-video-length video-path)
 (with-ffmpeg-video
  video-path
  (lambda (video)
   (let loop ((i 1)) (if (ffmpeg-next-frame! video) (loop (+ i 1)) i)))))

(define (video-length video-path)
 (let ((length-file (replace-extension video-path "video-length")))
  (or (if (file-exists? length-file)
         (let ((data (read-object-from-file length-file))
               (mod-time (file-change-time video-path)))
          (if (and (list? data) (= (length data) 2) (every number? data)
                 (= (second data) mod-time))
              (car data)
              #f))
         #f)
     ;; * matters because we want the time computed before the
     ;; video length to avoid race conditions
     (let* ((mod-time (file-change-time video-path))
            (l (compute-video-length video-path)))
      (write-object-to-file (list l mod-time) length-file)
      l))))

(define (video-first-frame video-path) 1)
(define (video-last-frame video-path) (video-length video-path))

(define (for-each-frame f v)
 (for-each-m-n f (video-first-frame v) (video-last-frame v)))
(define (for-each-frame-reversed f v)
 (for-each-m-n-dec f (video-last-frame v) (video-first-frame v)))
(define (map-frame f v)
 (map-m-n f (video-first-frame v) (video-last-frame v)))
(define (map-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))
(define (for-each-frame-indexed f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (video-last-frame v))))
(define (map-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (map-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))
(define (for-each-frame-indexed-but-last f v)
 (let ((first-frame (video-first-frame v)))
  (for-each-m-n (lambda (n) (f n (- n first-frame))) (video-first-frame v) (- (video-last-frame v) 1))))
(define (for-each-frame-but-last f v)
 (for-each-m-n f (video-first-frame v) (- (video-last-frame v) 1)))
(define (map-frame-but-last f v)
 (map-m-n f (video-first-frame v) (- (video-last-frame v) 1)))
(define (map-frame-pair individual-f pair-f video-path)
 (let ((first-frame (video-first-frame video-path))
       (last-frame (video-last-frame video-path)))
  (let loop ((n (+ first-frame 1))
	     (prev (individual-f first-frame))
	     (result '()))
   (if (> n last-frame)
       (reverse result)
       (let ((next (individual-f n)))
	(loop (+ n 1) next (cons (pair-f prev next) result)))))))
(define (for-each-frame-pair individual-f pair-f video-path)
 (let ((first-frame (video-first-frame video-path))
       (last-frame (video-last-frame video-path)))
  (let loop ((n (+ first-frame 1))
	     (prev (individual-f first-frame)))
   (unless (> n last-frame)
    (let ((next (individual-f n)))
     (pair-f prev next)
     (loop (+ n 1) next))))))

(define (map-imlib-frame-from-video-indexed f video)
 (with-ffmpeg-video
  video
  (lambda (ffmpeg-video)
   (map-frame-indexed
    ;; Frame-nr is the current frame number -- the first frame may not be zero
    ;; Index is the frame offset (starting at 0) in the video file
    (lambda (frame-nr index)
     (when (ffmpeg-video-finished? ffmpeg-video)
      (ffmpeg-error "ffmpeg video finished prematurely" ffmpeg-video))
     (let ((result (f frame-nr index (ffmpeg-video-frame-data-as-imlib ffmpeg-video))))
      (ffmpeg-next-frame! ffmpeg-video)
      result))
    video))))

(define (for-each-imlib-frame-from-video-indexed f video)
 (let ((ffmpeg-video (ffmpeg-open-video video)))
  (for-each-frame-indexed
   ;; Frame-nr is the current frame number -- the first frame may not be zero
   ;; Index is the frame offset (starting at 0) in the video file
   (lambda (frame-nr index)
    (when (ffmpeg-video-finished? ffmpeg-video)
     (ffmpeg-error "ffmpeg video finished prematurely" ffmpeg-video))
    (f frame-nr index (ffmpeg-video-frame-data-as-imlib ffmpeg-video))
    (ffmpeg-next-frame! ffmpeg-video))
   video)))

(define (for-each-imlib-frame-from-video-indexed-but-last f video)
 (let ((ffmpeg-video (ffmpeg-open-video video)))
  (for-each-frame-indexed-but-last
   ;; Frame-nr is the current frame number -- the first frame may not be zero
   ;; Index is the frame offset (starting at 0) in the video file
   (lambda (frame-nr index)
    (when (ffmpeg-video-finished? ffmpeg-video)
     (ffmpeg-error "ffmpeg video finished prematurely" ffmpeg-video))
    (f frame-nr index (ffmpeg-video-frame-data-as-imlib ffmpeg-video))
    (ffmpeg-next-frame! ffmpeg-video))
   video)))

(define (map-imlib-frame-pair-from-video individual-f pair-f video)
 ;; Note that you do not have to free the imlib data here!
 ;; unlike the non-pair version
 ;; individual-f :: frame-nr -> imlib -> a
 ;; pair-f :: a -> a -> b
 (let ((ffmpeg-video (ffmpeg-open-video video)))
  (map-frame-pair
   (lambda (frame-nr)
    (let ((frame-data (ffmpeg-video-frame-data-as-imlib ffmpeg-video)))
     (ffmpeg-next-frame! ffmpeg-video)
     (individual-f frame-nr frame-data)))
   pair-f
   video)))

(define (for-each-imlib-frame-pair-from-video-indexed individual-f pair-f video)
 ;; Note that you do not have to free the imlib data here!
 ;; unlike the non-pair version
 ;; individual-f :: frame-nr -> index -> imlib -> a
 ;; pair-f :: a -> a -> b
 (define (for-each-frame-pair-indexed individual-f pair-f video-path)
  (let ((first-frame (video-first-frame video-path))
	(last-frame (video-last-frame video-path)))
   (let loop ((n (+ first-frame 1))
	      (i 1)
	      (prev (individual-f first-frame 0)))
    (unless (> n last-frame)
     (let ((next (individual-f n i)))
      (pair-f prev next)
      (loop (+ n 1) (+ i 1) next))))))
 (let ((ffmpeg-video (ffmpeg-open-video video)))
  (for-each-frame-pair-indexed
   (lambda (frame-nr index)
    (let ((frame-data (ffmpeg-video-frame-data-as-imlib ffmpeg-video)))
     (ffmpeg-next-frame! ffmpeg-video)
     (individual-f frame-nr index frame-data)))
   pair-f
   video)))

(define (for-each-imlib-frame-pair-from-video individual-f pair-f video)
 (for-each-imlib-frame-pair-from-video-indexed
  (lambda (frame-nr index frame-data)
   (individual-f frame-nr frame-data))
  pair-f
  video))
)

(defun setup  ()
  (setq	*const::ang-eps*  0.0001
	*const::lin-eps*  0.1
	*acad*		  (vlax-get-acad-object)
	*active-document* (vla-get-activedocument *acad*)
	*ucs-collection*  (vlax-get-property
			    *active-document*
			    'UserCoordinateSystems)
	*model-space*	  (vla-get-modelspace *active-document*)
	*paper-space*	  (vla-get-paperspace *active-document*))

  (setvar "cmdecho" 0))
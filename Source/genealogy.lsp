#|
|		***** genealogy.lsp *****
|
|Author: Ian Beamer, Ryan Hinrichs
|Class:  CSC461 Programming Languages
|Instructor: Dr.John Weiss
|Date:   December 1, 2017
|
|Program: Lisp Family Database (program 3)
|Description: This program creates a database from a series of database
|entries to create a family tree, allowing the user to query the database
|with the following commands:
|(parents 'name)            (children 'name)            (siblings 'name)
|(mothers 'name)            (sons 'name)                (sisters 'name)
|(fathers 'name)            (daughters 'name)           (brothers 'name)
|(grandparents 'name)       (grandchildren 'name)       (uncles 'name)
|(grandfathers 'name)       (grandsons 'name)           (aunts 'name)
|(grandmothers 'name)       (granddaughters 'name)      (nieces 'name)
|(ancestors 'name)          (descendants 'name)         (nephews 'name)
|(male-ancestors 'name)     (male-descendants 'name)    (cousins 'name)
|(female-ancestors 'name)   (female-descendants 'name)  (male-cousins 'name)
|                                                       (female-cousins 'name)
|
|Usage: clisp -repl genealogy.lsp database.dat
|Bugs: If a database entry exists in a relationship but not as it's own entity,
|any functions that utilize that name will break.
|
|#

#|
Load the file containing query functions
|#
(load "familyfunc.lsp")

(defun main (file)
    "(main args): emulates a main function, called with command-line args"

    ; check for correct usage
    (when (null file) (return-from main "Usage: genealogy.lsp filename"))
    
    ; attempt to open file
    (setf fin (open file :if-does-not-exist nil)) ; return NIL on error
    (when (null fin) (return-from main (format nil "Could not open ~a~%" file)))

    ; read file, print contents
    ;(format t "People found in ~a~%:" file)

    (let (data (familyTree (list nil)))
        (do ((data (read fin nil) (read fin nil)))
            ((null data) (close fin))
            ;(format t "~a~%" data)

            (setq data (make-person 
                :name (car data)
                :gender (car (cdr data))
                :parents (car (cdr (cdr data)))
                :children (car (cdr (cdr (cdr data))))
                )
            )

            (setf familyTree(cons data familyTree))
        )

        (remove nil familyTree)
        ;(format t "Family: ~S~%" familyTree)
    )
)

; Main function needs to be called each time the program is loaded for 
; database setup
(setf *database* (main (car *args*)))

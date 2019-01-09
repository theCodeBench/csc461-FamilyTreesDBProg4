#|
		***** familyfunc.lsp *****

Contains all the family relations, and their helper functions.

Author: Ian Beamer, Ryan Hinrichs
Class:  CSC461 Programming Languages
Date:   Fall 2017

|#

;(format t "Loaded familyfunc")

(defstruct person name gender parents children)

#|
Finds if the name given is in the list given, will either return nil, 
return the name, or continues checking deeper in the list
|#
(defun find_name ( name list ) 
    "(find_name name list): finds the name if it is in the given list"
    (cond 
        ( ( null list ) nil ) ;if given list is empty, return nil
        ( (equal (person-name ( car list ) ) name ) (car list) ) ;if the car of the entry matches, return it
        (T (find_name name (cdr list) ) ) ;if isn't found, check farther in the list
    )
)

#|
Function that calls the find_name function on the database
|#
(defun base_find ( name )
    "(base_find name): makes find_name easier to use"
    (find_name name *database*)
)

#|
Function that filters a list based on the gender given
|#
(defun filter (family gender)
    "(filter family gender): filters a list based on the gender given"
    (cond
        ((null family) nil)
        ((equal (person-gender (car family)) gender) (cons (person-name (car family)) (filter (cdr family) gender)))
        (t (filter (cdr family) gender))
    )
)

#|
Function that flattens a list of lists into a single list
|#
(defun flatten (lst)
    "(flatten lst): turns a list of lists into a single list"
    (cond
        ((null lst) nil)
        ((atom lst) `( , lst))
        (t (apply #'append(mapcar #'flatten lst)))
    ) 
)

#|
Function that filters to only return people that are male
|#
(defun malefilter (person)
    "(malefilter person): calls filter on the male gender"
    (filter (mapcar #'base_find person) 'male)
)

#|
Function that filters to only return people that are female
|#
(defun femalefilter (person)
    "(femalefilter person): calls filter on the female gender"
    (filter (mapcar #'base_find person) 'female)
)

#|
Function that finds the parents of a given name
|#
(defun parents( name )
    "(parents name): finds the parents of the person"
    (let ( ( p_parent ( base_find name ) ) ) ; p_parent is the potential parent
        (cond                                ; check if actually a parent
            ( (null p_parent) nil )          ; nil if there's no name
            ( T (person-parents p_parent ) )
        ) 
    )
)

#|
Function that finds the mothers of a given name
|#
(defun mothers (name)
    "(mothers name): female filters (parents name)"
    (femalefilter (parents name))
)

#|
Function that finds the fathers of a given name
|#
(defun fathers (name)
    "(fathers name): male filters (parents name)"
    (malefilter (parents name))
)

#|
Function that finds the children of a given name
|#
(defun children( name )
    "(children name): finds the parents of the person"
    (let ( ( p_child ( base_find name ) ) )  ; p_child is the potential child
        (cond                                ; check if actually a child
            ( (null p_child) nil )           ; nil if there's no name
            ( T (person-children p_child ) ) 
        )
    )
)

#|
Function that finds the sons of a given name
|#
(defun sons (name)
    "(sons name): male filters (children name)"
    (malefilter (children name))
)

#|
Function that finds the daughters of a given name
|#
(defun daughters (name)
    "(daughters name): female filters (children name)"
    (femalefilter (children name))
)

#|
Function that finds the siblings of a given name
|#
(defun siblings (name)
    "(siblings name): finds the siblings of person"
    (let ((family (parents name)))
        (remove name (remove-duplicates (flatten (mapcar #'children family))))
    )
)

#|
Function that finds the sisters of a given name
|#
(defun sisters (name)
    "(sisters name): female filters (siblings name)"
    (femalefilter (siblings name))
)

#|
Function that finds the brothers of a given name
|#
(defun brothers (name)
    "(brothers name): male filters (siblings name)"
    (malefilter (siblings name))
)

#|
Function that finds the grandparents of a given name
Grandparents are the parents of parents of the given name
|#
(defun grandparents (name)
    "(grandparents name): finds the grandparents of the person"
    (let ((grandparent (parents name)))
        (remove-duplicates (flatten (mapcar #'parents grandparent)))
    )
)

#|
Function that finds the grandfathers of a given name
Grandparents are the parents of parents of the given name
|#
(defun grandfathers (name)
    "(grandfathers name): male filters (grandparents name)"
    (malefilter (grandparents name))
)

#|
Function that finds the grandmothers of a given name
Grandparents are the parents of parents of the given name
|#
(defun grandmothers (name)
    "(grandmothers name): female filters (grandparents name)"
    (femalefilter (grandparents name))
)

#|
Function that finds the grandchildren of a given name
Grandchildren are the children of the children of the given name
|#
(defun grandchildren (name)
    "(grandchildren name): finds the grandchildren of the person"
    (let ((grandchild (children name)))
        (remove-duplicates (flatten (mapcar #'children grandchild)))
    )
)

#|
Function that finds the grandsons of a given name
Grandchildren are the children of the children of the given name
|#
(defun grandsons (name)
    "(grandsons name): male filters (grandchildren name)"
    (malefilter (grandchildren name))
)

#|
Function that finds the granddaughters of a given name
Grandchildren are the children of the children of the given name
|#
(defun granddaughters (name)
    "(granddaughters name): female filters (grandchildren name)"
    (femalefilter (grandchildren name))
)

#|
Function for the aunt or uncle of a given name
|#
(defun aufind (name)
    "(aufind name): finds the aunts and uncles of the person"
    (remove-duplicates (append 
        (let ((au (parents name)))
            (flatten (mapcar #'siblings au))
        )
        (let ((au (cousins name)))
            (flatten (mapcar #'parents au))
        )
    ))
)


#|
Function that finds the uncles of a given name
Uncles are the male siblings of the parents of the given name
|#
(defun uncles (name)
    "(uncles name): male filters (aufind name)"
    (malefilter (aufind name))
)

#|
Function that finds the aunts of a given name
Aunts are the female siblings of the parents of the given name
|#
(defun aunts (name)
    "(aunts name): female filters (aufind name)"
    (femalefilter (aufind name))
)

#|
Function that finds the nieces of a given name
Nieces are the female children of the siblings of the given name
|#
(defun nieces (name)
    "(nieces name): finds the nieces of the person"
    (let ((niece (siblings name)))
        (remove-duplicates (femalefilter (flatten (mapcar #'children niece))))
    )
)

#|
Function that finds the nephews of a given name
Nephews are the male children of the siblings of the given name
|#
(defun nephews (name)
    "(nephews name): finds the nephews of the person"
    (let ((nephew (siblings name)))
        (remove-duplicates (malefilter (flatten (mapcar #'children nephew))))
    )
)

#|
Function that finds the cousins of a given name
Cousins are the children of the siblings of the parents of the given name
|#
(defun cousins (name)
    "(cousins name): finds the cousins of the person"
    (let ((par_sib (apply #'append (mapcar #'siblings (parents name)))))
        (remove-duplicates (apply #'append (mapcar #'children par_sib)))
    )
)

#|
Function that finds the male cousins of a given name
Cousins are the children of the siblings of the parents of the given name
|#
(defun male-cousins (name)
    "(male-cousins name): male filters (cousins name)"
    (malefilter (cousins name))
)

#|
Function that finds the female cousins of a given name
Cousins are the children of the siblings of the parents of the given name
|#
(defun female-cousins (name)
    "(female-cousins name): female filters (cousins name)"
    (femalefilter (cousins name))
)

#|
Function that finds the ancestors of a given name
Ancestors are all names higher on the family tree of the given name
|#
(defun ancestors (name)
    "(ancestors name): finds the ancestors of the person"
    (let ((ancestor (parents name)))
        (remove-duplicates (flatten (list ancestor (mapcar #'ancestors ancestor))))
    )
)

#|
Function that finds the male ancestors of a given name
Ancestors are all names higher on the family tree of the given name
|#
(defun male-ancestors (name)
    "(male-ancestors name): male filters (ancestors name)"
    (let ((maleancestor (ancestors name)))
        (malefilter maleancestor)
    )

    ;(malefilter (ancestors name))
)

#|
Function that finds the female ancestors of a given name
Ancestors are all names higher on the family tree of the given name
|#
(defun female-ancestors (name)
    "(female-ancestors name): female filters (ancestors name)"
    ;(let ((femaleancestor (ancestor name)))
    ;    (femalefilter femaleancestor)
    ;)

    (femalefilter (ancestors name))   
)

#|
Function that finds the descendants of a given name
Descendants are all names lower on the family tree of the given name
|#
(defun descendants (name)
    "(descendants name): finds the descendants of the person"
    (let ((descendant (children name)))
        (remove-duplicates (flatten (list descendant (mapcar #'descendants descendant))))
    )
)

#|
Function that finds the male descendants of a given name
Descendants are all names lower on the family tree of the given name
|#
(defun male-descendants (name)
    "(male-descendants name): male filters (descendants name)"
    (malefilter (descendants name))
)

#|
Function that finds the female descendants of a given name
Descendants are all names lower on the family tree of the given name
|#
(defun female-descendants (name)
    "(female-descendants name): female filters (descendants name)"
    (femalefilter (descendants name))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/permutations
;;; NAME 
;;; permutations
;;;
;;; File:             permutations.lsp
;;;
;;; Class Hierarchy:  none, no classes defined.
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Various permutation functions.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    10th November 2002
;;;
;;; $$ Last modified: 12:55:58 Sat Jan 14 2012 ICT
;;;
;;; SVN ID: $Id$
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 2 of the License, or (at your
;;;                   option) any later version.
;;;
;;;                   slippery-chicken is distributed in the hope that it will
;;;                   be useful, but WITHOUT ANY WARRANTY; without even the
;;;                   implied warranty of MERCHANTABILITY or FITNESS FOR A
;;;                   PARTICULAR PURPOSE.  See the GNU General Public License
;;;                   for more details.
;;;
;;;                   You should have received a copy of the GNU General Public
;;;                   License along with slippery-chicken; if not, write to the
;;;                   Free Software Foundation, Inc., 59 Temple Place, Suite
;;;                   330, Boston, MA 02111-1307 USA
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns the elements of <list> permutated and as a flat list, unless 
;;; <sublists> is t whereupon the the result is a list of lists each one 
;;; a permutation of <list>

;;; ****f* permutations/inefficiently-permutate
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;;
;;;
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defun inefficiently-permutate (list &key (max nil) (skip 0) (fix t)
                                          (sublists nil))
;;; ****
  (let ((permutations (inefficient-permutations (length list)
                                                :max max
                                                :skip skip
                                                :fix fix)))
    (if sublists
        (loop for l in permutations collect
              (loop for e in l collect (nth e list)))
      (loop for p in (flatten permutations) collect (nth p list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This function ensures a mixed bag of permutations (i.e. avoiding all the 0s
;;; at the beginning first, then all the 1s etc.).  We return simply a list of
;;; <max> permutations of the numbers less than <level>, i.e. we don't
;;; permutate a given list.  It is inefficient in so far as it simply shuffles
;;; the numbers and so always has to check whether we have the shuffle already
;;; before we store it.  The order of the permutations (and therefore the
;;; result) returned will always be the same unless <fix> is set to nil.
;;; <skip> allows you to skip a number of permutations, which only makes sense
;;; if fix is t.

;;; SAR Sun Jan 15 21:34:41 GMT 2012: Added robodoc info

;;; ****f* permutations/inefficient-permutations
;;; FUNCTION
;;; Return a shuffled, non-systematic list of permutations of all possible
;;; permutations of a set of consecutive integers beginning with zero. The
;;; function's first argument, <level>, is an integer that determines how many
;;; consecutive integers from 0 are to be used for the process. An optional
;;; keyword argument <max> allows the user to specify the maximum number of
;;; permutations to return. 
;;;
;;; This function differs from the "permutations" function in that it's result
;;; is not ordered systematically. 
;;;
;;; The function simply returns a list of <max> permutations of the numbers
;;; less than <level>, i.e. it doesn't permutate a given list.  
;;;
;;; The function is inefficient in so far as it simply shuffles the numbers and
;;; so always has to check whether the new list already contains the shuffled
;;; before storing it.
;;; 
;;; The order of the permutations returned will always be the same unless <fix>
;;; is set to NIL. Keyword argument <skip> allows the user to skip a number of
;;; permutations, which only makes sense if :fix is set to T.
;;;
;;; ARGUMENTS 
;;; An integer that indicates how many consecutive integers from 0 are to be
;;; used for the process. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :max. An integer that indicates the maximum number of
;;;   permutations to be returned.
;;; - keyword argument :skip. An integer that indicates a number of
;;;   permutations to skip.
;;; - keyword argument :fix. T or NIL to indicate whether the given sequence
;;;   should always be shuffled with the same (fixed) random seed (thus always
;;;   producing the same result). T = fixed seed. Default = T.
;;;
;;; RETURN VALUE  
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Creating a shuffled, non-systematic list of all permutations of consecutive
;; integers 0 to 4 
(inefficient-permutations 4)

=> ((2 3 0 1) (3 1 2 0) (2 0 3 1) (1 0 2 3) (1 2 3 0) (0 2 3 1) (2 1 0 3)
    (0 1 2 3) (2 3 1 0) (1 2 0 3) (3 0 1 2) (3 1 0 2) (1 3 2 0) (1 0 3 2)
    (2 0 1 3) (3 2 1 0) (2 1 3 0) (3 2 0 1) (1 3 0 2) (0 2 1 3) (3 0 2 1)
    (0 1 3 2) (0 3 2 1) (0 3 1 2))

;; Using 0 to 4 again, but limiting the number of results returned to a maximum
;; of 7
(inefficient-permutations 4 :max 7)

=> ((2 3 0 1) (3 1 2 0) (2 0 3 1) (1 0 2 3) (1 2 3 0) (0 2 3 1) (2 1 0 3))

;; The same call will return the same "random" results each time by default
(loop repeat 4 do (print (inefficient-permutations 3 :max 5)))

=>
((2 0 1) (2 1 0) (0 2 1) (1 0 2) (1 2 0)) 
((2 0 1) (2 1 0) (0 2 1) (1 0 2) (1 2 0)) 
((2 0 1) (2 1 0) (0 2 1) (1 0 2) (1 2 0)) 
((2 0 1) (2 1 0) (0 2 1) (1 0 2) (1 2 0))

;; Setting the :fix argument to NIL will result in differnt returns
(loop repeat 4 do (print (inefficient-permutations 3 :max 5 :fix nil)))

=>
((1 0 2) (0 1 2) (1 2 0) (2 1 0) (0 2 1)) 
((1 2 0) (2 0 1) (2 1 0) (1 0 2) (0 1 2)) 
((0 1 2) (1 0 2) (2 0 1) (1 2 0) (2 1 0)) 
((0 2 1) (1 2 0) (0 1 2) (2 0 1) (1 0 2))

|#
;;; SYNOPSIS
(defun inefficient-permutations (level &key (max nil) (skip 0) (fix t))
;;; ****
  (let* ((result '())
         (natural-max (loop for i from 2 to level with j = 1 do 
                           (setf j (* j i)) 
			 finally (return j)))
         (num-perms (+ skip
                       (if max 
			   max
			   natural-max)))
         (start (loop for i below level collect i))
         (current '())
         (reset fix)
         (count 0))
    (when (and max (> max natural-max))
      (error "inefficient-permutations: the number of possible ~
               permutations with <level> = ~a is ~a so can't return ~
               you the amount you requested (~a)"
             level natural-max max))
    (unless max
      (setf max num-perms))
    (loop while (<= count max) do
         (setf current (shuffle start :fix fix :reset reset))
       ;; reset random state only the first time
         (when fix
           (setf reset nil))
       ;; it's got to be an equal test for lists!
         (unless (member current result :test #'equal)
           (when (> count skip)
             (push current result))
           (incf count)))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 22:26:30 GMT 2012: Added robodoc info

;;; ****f* permutations/permutations
;;; FUNCTION
;;; Systematically produce a list of all possible permutations of a set of
;;; consecutive integers beginning with zero. The function's only argument,
;;; <level>, is an integer that determines how many consecutive integers from 0
;;; are to be used for the process.
;;;
;;; This is a more efficient permutation algorithm, but the results will always 
;;; be in a certain order, with the same number at the end until that
;;; permutation is exhausted, then the number below that etc. 
;;; 
;;; ARGUMENTS 
;;; An integer that indicates how many consecutive integers from 0 are to be
;;; used for the process. 
;;; 
;;; RETURN VALUE  
;;; A list of sequences (lists), each of which is a permutation of the
;;; original. 
;;; 
;;; EXAMPLE
#|
;; Produce a list consisting of all permutations that can be made of 4
;; consecutive integers starting with 0 (i.e., (0 1 2 3))
(permutations 4)

=>
((0 1 2 3) (1 0 2 3) (0 2 1 3) (2 0 1 3) (1 2 0 3) (2 1 0 3) (0 1 3 2)
 (1 0 3 2) (0 3 1 2) (3 0 1 2) (1 3 0 2) (3 1 0 2) (0 2 3 1) (2 0 3 1)
 (0 3 2 1) (3 0 2 1) (2 3 0 1) (3 2 0 1) (1 2 3 0) (2 1 3 0) (1 3 2 0)
 (3 1 2 0) (2 3 1 0) (3 2 1 0))

|#
;;; SYNOPSIS
(defun permutations (level)
;;; ****
  (if (> level 8)
      (let ((stream (open "permutations.txt"
                          :direction :output :if-exists :overwrite
                          :if-does-not-exist :create)))
        (warn "I doubt you have the system requirements to return the ~a ~%~
               results of this operation so I'm writing them into the file ~%~
               'permutations.txt'." 
              (loop for i from 2 to level with j = 1 do 
                (setf j (* j i)) 
                finally (return j)))
        (permutations-aux level stream)
        (close stream))
    (permutations-aux level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun permutations-aux (level &optional (stream nil) (result '())
                               (current '()))
  ;;(format t "~&result: ~a current: ~a" result current)
  (if (= (length current) level)
      (if stream 
          (print current stream)
        (push current result))
    (loop for i below level do
      (unless (member i current)
        (setf result (permutations-aux level stream result 
                                       (cons i current))))))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 14 22:25:23 GMT 2012: Added robodoc info

;;; ****f* permutations/permutate
;;; FUNCTION
;;; Systematically produce a list of all possible permutations of an original
;;; list of elements of any type.
;;;
;;; NB: Such lists can quickly become very long, so slippery-chicken
;;; automatically defaults to printing the resulting list to a file and
;;; printing a warning above a certain length. 
;;; 
;;; ARGUMENTS 
;;; - A list with elements of any type.
;;; 
;;; RETURN VALUE  
;;; A list of lists that are all possible permutations of the original,
;;; specified list.
;;; 
;;; EXAMPLE
#|
(permutate '(a b c))

=> ((A B C) (B A C) (A C B) (C A B) (B C A) (C B A))


|#
;;; SYNOPSIS
(defun permutate (list)
;;; ****
  (loop for p in (permutations (length list)) collect
      (loop for e in p collect (nth e list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns a random number below <below> using a fixed seed (so you
;;; always get the same result for the same arguments), which is reset if the
;;; second arg is t
;;;
;;; to get a random state evaluate (make-random-state t)
;;;
;;; 10/4/07: seems to me that this is only true for random integers: calling
;;; this at startup with floats produces different results to when later called
;;; on the command line!

(let* ((state 
        #+allegro
         #S(random-state 
            :mti 624
            :fixseed #(3076891201 2816555789 443085417 1824682773
                       1789081809 3882359901 3022811001 4172769509
                       3826753633 2054249133 688743817 4083893173
                       2735368433 2140882429 1474421913 3172520837
                       2200183425 4231081549 2701720745 1706027093
                       1157520657 2273010589 628800953 4238692901
                       129214625 4077860333 2664970185 1707270389
                       1131386161 1075770685 3766188761 2197251269
                       3573461697 835318157 294098665 2200350101
                       2858324305 3429661405 2952305657 637111141
                       2689450209 300933421 1823709705 3299724853
                       551276913 1260024957 4036403481 4024845829
                       318329601 769623245 2592653609 1920647893
                       2869417361 816800285 1183451705 2303202469
                       2892622113 1523014765 767790153 536873845
                       2886933937 3783377853 4019672921 3807999813
                       3826778945 3802523661 3551558505 4127205397
                       675162577 2344098141 1427310713 539291109
                       2341216609 4266239917 103550601 1015912629
                       1188657649 1095295741 3722550681 3225745541
                       1885258625 2404457293 4255303081 621469013
                       340102673 1375379613 7837369 153360165
                       1043884449 439010029 3809550537 3559552501
                       2213733937 3848524349 2497278937 2967259589
                       2698089409 1384077 1107533801 2884559509
                       2992767569 3572168669 1721480441 3352924261
                       2784149985 4179537453 3130442505 3584729909
                       1730372209 3318124925 115530265 3805604613
                       1601467393 3458593229 4084664873 201340885
                       3604448913 2197626653 3731055417 2228836773
                       3309251105 1389980013 3420517705 2766282869
                       2771316401 2539987133 2137116761 3376503877
                       3612042305 2679608589 3449881705 3683835157
                       652876497 665130589 934453625 1303868133
                       4288782945 2349790381 3742611337 1520758197
                       3822684913 204701693 3803862681 1547050373
                       3025822849 1744701517 891653801 100323925
                       1490927377 727113117 4218253241 1426578469
                       1503537825 4134752237 2236811721 80153333
                       4192679729 321236797 3998248153 1489448645
                       1671787713 2804763533 2007541993 193730453
                       1945531217 3548804317 2876755449 845059429
                       3099116257 280656685 1489169417 3860625461
                       790360945 405778045 2022183705 2164822021
                       1395692801 2860080845 198071081 1100655829
                       276314001 2192038941 45464633 575643301
                       594897697 3331880557 1015504457 3061396853
                       2084290481 1145404861 2965719385 3691921733
                       838845761 3424010765 3510275433 310022677
                       2544307153 4163834717 889928313 1181677541
                       22553441 2965455277 2630115465 3803267765
                       3406470129 3176865021 1600917401 4125901445
                       806815105 3006788941 1351702441 1031784277
                       2210852881 2425363101 794577081 3847266597
                       1524951969 1424547053 2929586889 3432554485
                       910985265 3865349181 635463129 532059077
                       1048204737 2484238477 4218860009 385764501
                       2686182481 2235494877 3616335609 2861079141
                       429900769 1707296813 2956465417 184765749
                       1237681265 2673199997 3196472345 2944480517
                       1328395777 1876544461 1821285417 3360301525
                       1223288977 749705501 1283527993 3869959077
                       1208790049 50627437 683067209 2848279157
                       1111068849 2233653949 1019331161 1080043077
                       2503288385 1421995789 2809992809 2411153173
                       2876570833 978718813 639423353 3477830885
                       1370465377 4183854765 675154313 1773311925
                       1298967793 834648573 1291241625 4066862981
                       3298076289 2650726989 1491476649 10075221
                       100737297 4272314269 3041141177 3204343333
                       1124904097 42691053 2280774601 10433781
                       3391281457 2082498877 1955159769 2863331525
                       1380988609 868525453 376290025 1128628629
                       3889321297 2803134173 1138425849 1968676709
                       161989857 87627053 704003593 1499406901
                       2283804017 3090737277 1373472025 1396627973
                       3026966273 3410442445 2662851881 1427019477
                       1898748305 2115261981 1422224953 1458251941
                       3020221729 1428000877 963588169 3552993141
                       136864177 4143790013 3696704345 456592197
                       2716567361 649763853 426286953 1212742677
                       2471751121 723127645 3823594617 2907505125
                       2980586849 4075609005 1786819209 2225663157
                       3154099697 1180784381 2757394841 3064433797
                       1389578113 1431490381 1461971369 2219356501
                       1351373329 4070151325 2787441337 3876662053
                       320171425 3428513517 978240713 1977272821
                       1181362737 4049159741 323704793 2661572037
                       3223787457 4081308301 294503401 90289813
                       4229547601 4127648733 1231165689 3620447333
                       3385902561 4229685805 788358921 3861903157
                       3140200049 3218670973 2878193177 1363246853
                       3823826945 1774298573 726281769 2632453077
                       2051033745 1943409437 3028469561 4161826213
                       3898486305 165911917 397449545 2306634869
                       3741856433 919901373 1216722009 2442326085
                       4179814465 788547853 4026345577 825209109
                       2151931601 125504093 1198196089 2775811813
                       3795953249 711338157 1284266889 3365159349
                       1740885745 3628069885 1586969241 2813112709
                       3151161473 104053837 1414181545 4253853269
                       3563619089 3916026269 452907961 1653141541
                       3422498465 2136441837 4004818377 21716725
                       1008892721 1661936445 992366809 2695021253
                       2835282113 1066401677 903269609 3528649621
                       2381461329 789997789 1092760057 384084325
                       2602223329 1466674989 676171785 3329608757
                       3018340209 322314877 1150744345 2391352325
                       1051400449 4165538509 2605020969 1423343829
                       1428487057 183816221 79241273 1327149733
                       1712877345 1851173485 1820000841 535267701
                       3621323697 3785945533 978136409 3363034437
                       1004226881 1519580685 4097487209 1064003093
                       2739195857 209258333 698850937 2092895205
                       2759599969 751596973 3076588681 3396638389
                       2713247729 3294335229 1957491609 712431237
                       3767765377 3718359373 1499102121 2707790677
                       43365393 1612123805 751938745 912635173
                       1858727841 3900772589 3458438857 2012279797
                       1011600433 3997302845 622479833 1436952517
                       769120705 2242456717 3427325417 521740437
                       1314629713 256042461 2216381177 2007150181
                       3196438497 606633005 2129049865 254845237
                       1129695345 256917373 2516135961 4027959557
                       632044033 601718733 2007613481 836367829
                       4074417297 1081117981 3731388729 3775526821
                       2922623009 3480663917 3771624265 3959922293
                       60478641 2491043517 1789765209 3839474245
                       185903681 2524095245 4011932265 1744575253
                       760660177 1997800541 1671247737 4163866853
                       3109529697 2267005613 2482941321 524938165
                       3135172849 3887345149 3751521433 2751855493
                       2719296129 144479821 1867728041 2765328469
                       1276372241 3550563229 4103964089 1739029029
                       4235571361 3570900461 26968009 2932574453
                       3622182193 2951863613 170345177 1655606469
                       1873918657 848255373 501473001 1622431125
                       3998619985 1401709277 1800233977 1057338213
                       1964099809 1867663661 2613633545 3579868725
                       980703601 287792253 414476569 1525116421
                       4193147649 2575232205 1232537897 3908201173
                       1147231633 290015773 3666924089 853425317
                       1102049569 2051261549 497734729 1121760117
                       1934468529 3964185533 2460426073 197435205
                       131009345 3483324429 2846933865 2682376213
                       1333375441 2219573597 3461075065 3703903717
                       3788777825 3328184237 3412416137 1544831157
                       70648305 524929789 2556650905 2035949701
                       3780627329 3022291789 2671054249 1020691797
                       568530449 3238561949 2338479801 4216209189
                       1979871649 291187437 2988206281 2061180405
                       2683399729 3307125309 592264153 1824256453
                       2408356801 3007481485 1940383721 203721365
                       518097489 3102924765 1337490681 2987243621
                       4290693601 1172903469 3891530505 772098869
                       1782835825 1975220605 1170776601 3019772677))
         #+clisp
         #S(RANDOM-STATE
            #*0110011101100001010001011000101100100110001110110111010111110001)
         #+sbcl
         #S(RANDOM-STATE 
            :STATE #.(MAKE-ARRAY 
                      627 :ELEMENT-TYPE '(UNSIGNED-BYTE 32)
                      :INITIAL-CONTENTS
                      '(0 2567483615 1 3470661576 114701096
                        2370305800 3382878568 1523943496
                        489802152 2982413192 1370274792
                        3905241288 2869364776 1479774216
                        3483549288 1197850952 412380840
                        2804098184 3297192168 1714915784
                        910196008 891765000 3485760360
                        3590527560 2736370600 2640078216
                        430781928 2438525640 3679883816
                        2615611912 2784745576 2366739272
                        1859492008 846448264 240313064
                        2429385672 3751626536 1421280008
                        616355176 3614780488 2824609192
                        2632796040 4264308712 4156105928
                        3941112872 2287670280 3841684072
                        2492589384 1187070632 3150768264
                        3010272488 1438641608 1468829992
                        3491185928 371961704 2823536200
                        1536755624 647408008 919185896
                        3339048648 2187142696 1169135112
                        1312918632 2332473160 1660382376
                        1028557448 2675300072 2217664456
                        447634216 2473068296 1504774504
                        3651588168 2473619880 947424136
                        3810894824 1918830792 1918119976
                        67409928 191768168 3851422024 773330600
                        957918344 2785874152 3006943688
                        3449988392 2462664968 216851304
                        1151754824 3464649640 1488121224
                        182459880 887405256 3010312744
                        4219083272 2921414760 1632492360
                        3133358248 3108720264 2194852584
                        1107444680 1024028456 3394964232
                        3045014888 174748744 862897576
                        2506477448 2644056040 297200840
                        1716110376 1815092232 804969064 56634696
                        3281578664 1342599304 3647407336
                        1470543304 1568848168 1044204808
                        1091049320 2484274760 2429923240
                        2226205064 1928368616 3556089544
                        3548925480 2655428104 4070241384
                        417791816 2872644776 392827528 896122600
                        3813124040 1269728040 4203745032
                        86471016 2462062664 1565989288
                        1152717704 1250331624 247517384
                        1808357416 3789398024 3215036008
                        922898760 2084814504 2979411080
                        175831272 2617579976 1278004520
                        326324488 3212659560 4287736392
                        3079865256 2073130376 3422225896
                        490242760 3349996072 2400526856
                        3366889576 986850120 3914918056
                        1219155592 3023745768 16714696
                        3416102696 2768704264 2510923112
                        463977544 1713991080 1466324872
                        2263744488 578598088 2815618088
                        101524488 2808234600 1232500040
                        1293456040 2395469960 1984490728
                        1498774984 1587601704 3547026696
                        357334888 1881296456 3619313576
                        2387851656 4076829160 162358984
                        4108023336 2934285832 1566334056
                        3490662216 2854403240 3188562568
                        2084939496 3022548936 3252071208
                        3401586440 872798568 3458293832
                        585482664 1585027976 1875866600
                        2246744264 3211165736 4190021640
                        1413281384 2210175304 2870437544
                        2560343176 3654344936 3902267848
                        3655267624 3206895880 1333113704
                        1321529928 105622440 2381839752
                        1157492200 307514056 1075055144
                        1589127688 1571033192 1637772104
                        2727776424 1754422920 2324339432
                        2512638920 3009003304 3971685128
                        758910312 1395459144 3901494696
                        1794039688 2720720872 4060773576
                        3615757352 1576152072 3006376552
                        2933177672 2336242344 1147016 1913836776
                        583812552 2196179240 2543934728
                        4210616168 2222560840 3628531624
                        3414049160 2666947048 808267464
                        240557608 2139926024 4135961704
                        4169109320 129262760 3107529352
                        1544130280 3201382344 2770785064
                        200810248 1313620328 3553273928
                        2350672296 231089032 989879272
                        2682020040 2502666280 1403498504
                        826302056 331276616 1653804712
                        1982416008 4228827368 1626515912
                        2662931752 2648662280 618011496
                        2051029576 1655460776 400983432
                        1575540200 3694662344 1151545896
                        1919103496 3573643368 498250568
                        2390505640 2701256328 3988947688
                        3360725960 473818920 2843157256
                        3828774248 4177153096 1654046120
                        1476355976 3612332008 1214268616
                        492649512 2078225416 3144225384
                        2071659848 638575272 736299144
                        3032792296 2081099208 4065669416
                        2464113928 1510821736 421060168
                        981182376 3291531656 1591036392
                        259323592 1207551528 406566408 639052904
                        3661092680 1810762920 2431429256
                        3166009064 3410100168 496958248
                        3325568776 3153766760 3954962504
                        1090195880 3667569544 2484683752
                        613820616 353947688 4153980936
                        3198928488 789129544 1253488296
                        3527330952 1496625384 3440734664
                        3267052840 3080808712 2812183400
                        3389227592 1958017960 2707230088
                        4273716712 1122228936 4250556968
                        3524672008 2529615976 3071212360
                        1426710696 2046389896 3319950056
                        1621451720 776605480 3812305672
                        580443496 1427084360 2085184936
                        2794975112 240957416 3984461000
                        2707317800 1741962248 671644264
                        4150873416 3313994408 2587660424
                        643736808 756143560 3572193576
                        3441782024 2592716648 1979721288
                        2790805416 7033224 446444008 1870970568
                        3385126440 2163392008 1410372712
                        3174571848 2127541416 3441963656
                        2252955368 2709178312 1396646696
                        25177864 3843101032 1576351816
                        3917593000 1291269000 1662659560
                        3692556488 1731065896 3985752072
                        1686056552 496726344 193093288 886854792
                        3645020392 4110432712 1843752232
                        342585608 1070525288 2249116232
                        3831866280 2992346504 259433960
                        240624328 2453242408 2245083656
                        183781480 1974682440 2654963880
                        2071525000 4209660648 650259400
                        293484328 2718380808 1348683112
                        2943147080 3718516136 3736631176
                        793878504 2841892040 2340916264
                        701580296 1628430952 1788843336
                        296170152 3541964936 2933951720
                        4259356104 1386840360 1316189448
                        611196776 3811536456 3286038440
                        137222536 3125484008 408717000
                        3149403688 3249653256 3899784296
                        3917506376 3898170536 4125866632
                        2697283304 159092680 1845971752
                        3318757128 831486312 1920368712
                        766534056 3973823368 2416120808
                        2388768968 3010142248 1033029640
                        2327484008 662026568 1291191976
                        637656200 1681424616 2696085960
                        3359085864 3158145288 1432834920
                        4133622344 1505643432 3538034056
                        2014940648 110324424 725658152
                        2509523464 2575937640 2711587656
                        303902888 763395720 1960458984
                        3802592200 3990511400 3990567688
                        3583356264 1343366216 782677416
                        2288058248 573474792 1142004936 64533544
                        3386264584 3464448616 488496456
                        2993609384 1585946760 910849256
                        3061278152 2475665704 516521224
                        1606059880 2829497928 990486440
                        1668833672 635568616 3522973384
                        1171471912 3799567880 1262183528
                        2802888520 1351287976 2469872264
                        4096339688 3410253768 2516022056
                        455622408 158720360 1900005448
                        3045525928 1112032136 47447016 57901256
                        567298088 4019965960 2573074024
                        2492989768 3071389352 766469256
                        3793119464 2553137608 4189175080
                        3071770888 1348975464 1660770888
                        2092889000 2331026824 547649000
                        4086772424 4033860136 157241864
                        2860980328 2194920264 1487067304
                        403703432 464659176 1548991432
                        3948840744 3468116744 439358824
                        2130668616 391208360 730957704
                        3472060392 2240242888 844225576
                        1340298248 3629560424 1457792328
                        1339982504 3296274568 2761710824
                        517352904 3214790952 1176995080
                        3027169128 241565256 3017689000
                        2588600712 1163979240 1684281032
                        2417387048 3947353608 3832335464
                        1038677832 1600433320 755681928
                        1752504040 2933202888 4077886248
                        159926024 3569633640 2723221576
                        688239016 3577531272 2743919592 55396552
                        3663556648 195876872 4167657064
                        2902608200 4057290408 3554995336
                        987517160 2742063560 710141224 217680136
                        2563777384 333488712 4117207976
                        1653026184 4044840424 2640509640
                        164035112 3912391176 2783740008
                        1632639816 433094824 3274149512
                        3614575336 1539867592 839561000
                        1285245704 2251455848 2218046536
                        1067713960 1347030920 497047528
                        902114504 1056113704 3287833608
                        3868596840 1609723208 2448829096
                        2364715144 3788915944)))
         #+ccl
         #.(CCL::INITIALIZE-MRG31K3P-STATE 2022741898 1381830813 2119763740
                                           188361518 1311787362 653778830)
         #+(and openmcl (not ccl))
         #.(RANDOM-STATE 27764 35226)
         )
       (current-state 
        (if state
            (make-random-state state)
            (error "permutations::random-rep: Repeatable random-state not ~
                    yet implemented for this lisp."))))
  ;; ****f* permutations/random-rep
  ;; FUNCTION
  ;;
  ;; 
  ;; ARGUMENTS 
  ;; 
  ;; 
  ;; OPTIONAL ARGUMENTS
  ;; 
  ;;
  ;; RETURN VALUE  
  ;; 
  ;; 
  ;; EXAMPLE
  #|

  |#
  ;; SYNOPSIS
  (defun random-rep (below &optional (reset nil))
    ;; ****
    (when reset
      (setf current-state (make-random-state state)))
    (random below current-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Modified from Common Music's shuffle function.

;;; SAR Sun Jan 15 17:01:34 GMT 2012: Added robodoc info

;;; ****f* permutations/shuffle
;;; FUNCTION
;;; Create a random ordering of a given sequence or a subsequence of a given
;;; sequence. 
;;;
;;; NB: The order of the permutations returned will always be the same unless
;;; keyword argument :fix is set to NIL.  
;;;  
;;; ARGUMENTS 
;;; - A sequence.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :start. A zero-based index integer indicating the first
;;;   element of a subsequence to be shuffled. Default = 0.
;;; - keyword argument :end. A zero-based index integer indicating the last
;;;   element of a subsequence to be shuffled. Default = the length of the
;;;   given sequence.
;;; - keyword argument :copy. T or NIL to indicate whether the given sequence
;;;   should be copied before it is modified or shoudl be destructively
;;;   shuffled. T = copy. Default = T.
;;; - keyword argument :fix. T or NIL to indicate whether the given sequence
;;;   should always be shuffled with the same (fixed) random seed (thus always
;;;   producing the same result). T = fixed seed. Default = T.
;;; - keyword argument :reset. T or NIL to indicate whether the random state
;;;   should be reset before the function is performed. 
;;;   T = reset. Default = T. 
;;;
;;; RETURN VALUE  
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Simple shuffle with default keywords.
(shuffle '(1 2 3 4 5 6 7))

=> (5 4 3 6 7 1 2)

;; Always returns the same result by default.
(loop repeat 4 do (print (shuffle '(1 2 3 4 5 6 7))))

=>
(5 4 3 6 7 1 2) 
(5 4 3 6 7 1 2) 
(5 4 3 6 7 1 2) 
(5 4 3 6 7 1 2)

;; Set keyword argument :fix to NIL to return different results each time 
(loop repeat 4 do (print (shuffle '(1 2 3 4 5 6 7) :fix nil)))

=>
(1 2 6 3 5 4 7) 
(1 3 5 2 7 4 6) 
(4 7 2 5 1 6 3) 
(1 5 3 7 4 2 6)

;; Set keyword arguments :start and :end to shuffle just a subsequence of the
;; given sequence
(loop repeat 4 
   do (print (shuffle '(1 2 3 4 5 6 7) 
		      :fix nil
		      :start 2
		      :end 5)))

=>
(1 2 5 4 3 6 7) 
(1 2 3 5 4 6 7) 
(1 2 4 5 3 6 7) 
(1 2 3 4 5 6 7)

|#
;;; SYNOPSIS
(defun shuffle (seq &key 
		(start 0) 
		(end (length seq))
		(copy t)
		(fix t)
		(reset t)
		&aux (width (- end start)))
;;; ****
  (if (< width 2)
      seq
      (progn
	(when (and copy (typep seq 'list))
	  (setf seq (copy-list seq)))
	;; call once just to initialize state
	(when (and fix reset)
	  (random-rep 10 t))
	(loop for i from start to (1- end)
	   for i2 = (+ start 
		       (if fix 
			   (random-rep width)
			   (random width)))
	   do 
	     (when (or (< i 0) (< i2 0))
	       (error "permutations::shuffle: indices < 0!"))
	     (rotatef (elt seq i) 
		      (elt seq i2)))
	seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Jan 15 21:13:57 GMT 2012: Added robodoc info

;;; ****f* permutations/multi-shuffle
;;; FUNCTION
;;; Applies the shuffle function a specified number of times to a specified
;;; list.  
;;; 
;;; NB: As with the plain shuffle function, the order of the permutations
;;; returned will always be the same unless the keyword argument :fix is set 
;;; to NIL.   
;;;  
;;; ARGUMENTS 
;;; - A sequence.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :start. A zero-based index integer indicating the first
;;;   element of a subsequence to be shuffled. Default = 0.
;;; - keyword argument :end. A zero-based index integer indicating the last
;;;   element of a subsequence to be shuffled. Default = the length of the
;;;   given sequence.
;;; - keyword argument :copy. T or NIL to indicate whether the given sequence
;;;   should be copied before it is modified or should be destructively
;;;   shuffled. T = copy. Default = T.
;;; - keyword argument :fix. T or NIL to indicate whether the given sequence
;;;   should always be shuffled with the same (fixed) random seed (thus always
;;;   producing the same result). T = fixed seed. Default = T.
;;; - keyword argument :reset. T or NIL to indicate whether the random state
;;;   should be reset before the function is performed. 
;;;   T = reset. Default = T. 
;;;
;;; RETURN VALUE  
;;; - A sequence.
;;; 
;;; EXAMPLE
#|
;; Simple multi-shuffle with default keywords.
(multi-shuffle '(a b c d e f g) 3)

=> (B A C E D G F)

;; Always returns the same result by default.
(loop repeat 4 do (print (multi-shuffle '(a b c d e f g) 3)))

=>
(B A C E D G F) 
(B A C E D G F) 
(B A C E D G F) 
(B A C E D G F)

;; Set keyword argument :fix to NIL to return different results each time 
(loop repeat 4 do (print (multi-shuffle '(a b c d e f g) 3 :fix nil)))

=>
(G C F B D E A) 
(A G F B D C E) 
(A B D G C F E) 
(G C A D E F B)

;; Set keyword arguments :start and :end to shuffle just a subsequence of the
;; given sequence
(loop repeat 4 
   do (print (multi-shuffle '(a b c d e f g) 3
			    :fix nil
			    :start 2
			    :end 5)))

=>
(A B D E C F G) 
(A B E C D F G) 
(A B E D C F G) 
(A B D C E F G)

|#
;;; SYNOPSIS
(defun multi-shuffle (seq num-shuffles &key 
		      (start 0) 
		      (end (length seq))
		      (copy t)
		      (fix t)
		      (reset t))
;;; ****
  (loop 
     with result = seq
     ;; repeat num-shuffles 
     for i below num-shuffles
     do
       (setf result (shuffle result :start start :end end :copy copy :fix fix
			     :reset reset))
     finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* permutations/multi-shuffle-with-perms
;;; FUNCTION
;;;
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defun multi-shuffle-with-perms (seq num-shuffles)
;;; ****
  (if (zerop num-shuffles)
      seq
    (let* ((len (length seq))
           (perms (inefficient-permutations len :max num-shuffles))
           (last (first (last perms))))
      (loop for i in last collect (elt seq i)))))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When a permutation results in something like (a b c) (c a b) the event c is
;;; repeated: check for this and move the offending element to the next place 
;;; that won't repeat (where possible).
;;; (move-repeats '((a b c) (c a b) (d e f) (a b c) (g h i))) ->
;;; ((A B C) (D E F) (C A B) (A B C) (G H I))
;;; Works with simple lists too:
;;; (move-repeats '(1 2 3 3 4 5 6 7 8 8 9 10)) ->
;;; (1 2 3 4 3 5 6 7 8 9 8 10)

;;; ****f* permutations/move-repeats
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;;
;;;
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defun move-repeats (list &optional (test #'eq))
;;; ****
  (let ((result (list (first list)))
        (rest (rest list)))
    (loop
       for last-last = (get-atom (first result) t)
       while rest do
       (loop for i in rest and j from 0 do
            (unless (funcall test last-last (get-atom i))
              (setf rest (remove i rest :start j :count 1))
              (push i result)
              (return))
          ;; this only gets triggered when we can't find a place for i.
            finally (warn "move-repeats: can't find non-repeating place! ~
                             present element: ~a, elements left: ~a"
                          i (length rest))
            (setf result (append rest result)
                  rest nil)))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-atom (x &optional last)
  (if (atom x)
      x
    (get-atom (first (if last
                         (last x)
                       x))
              last)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; e.g. (get-all-pairs '(1 2 3 4 5)) ->
;;; ((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5))

(defun get-all-pairs (list)
  (loop 
      with len = (length list)
      for e in (butlast list)
      for i from 0 
      append
        (loop for j from (1+ i) below len collect (list e (nth j list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; More permutation routines; found at 
;;; http://groups.google.com.ai/group/comp.lang.lisp/browse_thread/thread/
;;; e9313dd883b48008/ce4e2243623a16e7?lnk=raot on 6/7/09

;; Like lconc except that it does not
;; advance the cdr of the conc to the end of the list
;; until necessary.  for lconc and tconc the conc structure
;; usually is in a state where (cdr conc) is the last cons
;; cell of the list.   in the case of lazy-lconc and lazy-tconc
;; (cdr conc) is just some cons cell but will be advanced to the
;; end on demand whenevery anything needs to be added.
(defun lazy-lconc (conc list)
  (let ((ptr (cdr conc)))
    (if ptr
        (progn
          (loop while (cdr ptr)
                do (pop ptr))
          (setf (cdr ptr) list)
          (setf (cdr conc) ptr))
      (progn
        (setf (car conc) list)
        (setf (cdr conc) list)))))

;; Efficiently append a single item descructively to the end
;; of a conc-list.
(defun lazy-tconc (conc item)
  (lazy-lconc conc (list item)))

;; Remove the given element the first time it occurs
;; in the list consing as few cells as possible,
;; and only traversing as far as necessary into the list.
;; This function is completely non-destructive.
;; This is done by using tconc to collect the elements
;; of the list until we reach the unwanted item,
;; then using lconc to setf cdr to the remaining elements
;; without traversing any further.
;; An annoying side effect is that if the unwanted element
;; is not found then the entire list is re-allocated and then simply
;; thrown away for the garbage collector.
;; This does not matter for our application because we always
;; call remove-preserving-tail with an item that is for sure in
;; the list, but an in-general a safer implementation would save list
;; and return that value rather than returning nil in case the
;; item is unfound.
(defun remove-preserving-tail (item list)
  (declare (list list))
  (if (eql item (car list))
      (cdr list)
    (let ((conc (list nil)))
      (declare (list conc))
      (loop for sub on list
            do (if (eql item (car sub))
                   (progn (lazy-lconc conc (cdr sub))
                          (return-from remove-preserving-tail (car conc)))
                 (lazy-tconc conc (car sub))))
      list)))

;; Iteratively calculate all the permutations of LIMIT number
;; of elements from the list OBJECTS, and call the function VISIT
;; on each of them.
(defun apply-permutations (objects limit visit)
  (declare (function visit)
           (list objects))
  (labels ((apply-rec (limit remaining current-perm)
             (cond
               ((plusp limit)
                (dolist (i remaining)
                  (apply-rec (1- limit)
                             (remove-preserving-tail i remaining)
                             (cons i current-perm))))
               (t
                (funcall visit current-perm)))))
    (when (<= limit (length objects))
      (apply-rec limit objects nil))))

;; Find a permutation of a given list of objects which is a 4 element
;;  palindrome.
;; e.g., (find-first-permutation '(1 4 3 5 2 4 1 2)
;;                               4
;;                               (lambda (perm)
;;                                 (equal perm (reverse perm))))
;;        --> (1 4 4 1)
(defun find-first-permutation (objects limit predicate)
  (apply-permutations objects
                      limit
                      (lambda (perm)
                        (when (funcall predicate perm)
                          (return-from find-first-permutation perm)))))

;; Count the number of permutations of a given list which make
;; the given predicate TRUE.
;; e.g., count the number of symmetric permutations, (Palindromes)
;; (count-permutations '(1 2 2 3 3 2 2 4 4 1)
;;                     5
;;                   (lambda (perm)
;;                       (equal perm (reverse perm))))
;; --> 1152
(defun count-permutations (objects limit &optional (predicate
(constantly t)))
  (declare (optimize (speed 2))
           (function predicate))
  (let ((count 0))
    (declare (integer count))
    (apply-permutations objects
                        limit
                        (lambda (perm)
                          (when (funcall predicate perm)
                            (incf count))))
    count))

(defun list-permutations (objects limit &optional (predicate
(constantly t)))
  (let (perms)
    (apply-permutations objects
                        limit
                        (lambda (perm)
                          (when (funcall predicate perm)
                            (push perm perms))))
    perms))

#|
;; examples
;;   .
;;    .
;;     .
(defun print-it (obj)
  (fresh-line)
  (format t "~A" obj)
  (force-output))

(defun visit-and-print (objects)
  (apply-permutations objects
                      (length objects)
                      (lambda (x)
                        (print-it x)
                        (read-char)))) 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF permutations.lsp


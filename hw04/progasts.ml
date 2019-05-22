open Ast

let easy_p1_ast =
  [ Gfdecl
      (no_loc
         {frtyp= RetVal TInt; fname= "f"; args= []; body= [no_loc (Ret (Some (no_loc (CInt 0L))))]})
  ]

let easy_p2_ast =
  [ Gfdecl
      (no_loc
         { frtyp= RetVal TInt
         ; fname= "f"
         ; args= [(TInt, "x")]
         ; body=
             [ no_loc (Decl ("x", no_loc (CInt 0L)))
             ; no_loc
                 (Assn
                    ( no_loc (Id "x")
                    , no_loc
                        (Bop
                           ( IOr
                           , no_loc
                               (Bop
                                  ( Shl
                                  , no_loc
                                      (Bop
                                         ( Shr
                                         , no_loc
                                             (Bop
                                                ( Sub
                                                , no_loc
                                                    (Bop (Add, no_loc (Id "x"), no_loc (Id "x")))
                                                , no_loc
                                                    (Bop (Mul, no_loc (Id "x"), no_loc (Id "x")))
                                                ))
                                         , no_loc (Id "x") ))
                                  , no_loc (Id "x") ))
                           , no_loc
                               (Bop
                                  ( IAnd
                                  , no_loc (Id "x")
                                  , no_loc
                                      (Bop
                                         ( Sar
                                         , no_loc
                                             (Uop (Neg, no_loc (Uop (Bitnot, no_loc (Id "x")))))
                                         , no_loc (Id "x") )) )) )) ))
             ; no_loc (Ret (Some (no_loc (Id "x")))) ] }) ]

let easy_p3_ast =
  [ Gfdecl
      (no_loc
         { frtyp= RetVal (TRef RString)
         ; fname= "bar"
         ; args= [(TInt, "x"); (TRef RString, "y")]
         ; body=
             [ no_loc (Decl ("s", no_loc (CStr "This is a string")))
             ; no_loc (Decl ("array", no_loc (CArr (TInt, [no_loc (CInt 1L); no_loc (CInt 3L)]))))
             ; no_loc (Decl ("y", no_loc (Index (no_loc (Id "array"), no_loc (CInt 0L)))))
             ; no_loc (Ret (Some (no_loc (Id "s")))) ] })
  ; Gfdecl
      (no_loc
         { frtyp= RetVoid
         ; fname= "proc1"
         ; args= []
         ; body= [no_loc (SCall (no_loc (Id "proc2"), [])); no_loc (Ret None)] })
  ; Gfdecl
      (no_loc
         { frtyp= RetVoid
         ; fname= "proc2"
         ; args= []
         ; body= [no_loc (SCall (no_loc (Id "proc1"), [])); no_loc (Ret None)] })
  ; Gfdecl
      (no_loc
         { frtyp= RetVal TBool
         ; fname= "foo"
         ; args= [(TInt, "x"); (TRef (RArray TInt), "y")]
         ; body=
             [ no_loc
                 (Decl
                    ( "s"
                    , no_loc (Call (no_loc (Id "bar"), [no_loc (Id "x"); no_loc (CStr "cis341")]))
                    ))
             ; no_loc (SCall (no_loc (Id "proc1"), []))
             ; no_loc (Ret (Some (no_loc (CBool true)))) ] }) ]

let easy_p4_ast =
  [ Gfdecl
      (no_loc
         { frtyp= RetVal (TRef RString)
         ; fname= "f"
         ; args= []
         ; body=
             [ no_loc
                 (Decl
                    ( "s"
                    , no_loc
                        (CArr
                           ( TRef (RArray (TRef RString))
                           , [ no_loc
                                 (CArr
                                    ( TRef RString
                                    , [ no_loc (CStr "s00:\n+\n=2*\n")
                                      ; no_loc (CStr "s01:this is not a comment in string.*")
                                      ; no_loc (CStr "s02:\"\\t\\n\\\\?\"") ] ))
                             ; no_loc
                                 (CArr
                                    ( TRef RString
                                    , [ no_loc (CStr "s10:\133\134")
                                      ; no_loc (CStr "s11")
                                      ; no_loc (CStr "s12") ] )) ] )) ))
             ; no_loc
                 (Ret
                    (Some
                       (no_loc
                          (Index
                             (no_loc (Index (no_loc (Id "s"), no_loc (CInt 0L))), no_loc (CInt 1L))))))
             ] })
  ; Gfdecl
      (no_loc
         { frtyp= RetVal (TRef (RArray (TRef (RArray TInt))))
         ; fname= "g"
         ; args= [(TRef (RArray (TRef (RArray TInt))), "x")]
         ; body=
             [ no_loc
                 (Decl
                    ( "y"
                    , no_loc
                        (CArr
                           ( TRef (RArray TInt)
                           , [ no_loc (CArr (TInt, [no_loc (CInt 0L); no_loc (CInt 1L)]))
                             ; no_loc (CArr (TInt, [no_loc (CInt 2L); no_loc (CInt 3L)])) ] )) ))
             ; no_loc (Decl ("i", no_loc (CInt 0L)))
             ; no_loc
                 (Assn
                    ( no_loc
                        (Index
                           (no_loc (Index (no_loc (Id "x"), no_loc (CInt 0L))), no_loc (CInt 0L)))
                    , no_loc
                        (Bop
                           ( Add
                           , no_loc (Id "i")
                           , no_loc
                               (Index
                                  ( no_loc (Index (no_loc (Id "y"), no_loc (CInt 1L)))
                                  , no_loc (CInt 1L) )) )) ))
             ; no_loc
                 (Assn
                    ( no_loc (Id "i")
                    , no_loc
                        (Uop
                           ( Neg
                           , no_loc
                               (Uop
                                  ( Lognot
                                  , no_loc
                                      (Uop
                                         ( Bitnot
                                         , no_loc
                                             (Index
                                                ( no_loc (Index (no_loc (Id "x"), no_loc (CInt 0L)))
                                                , no_loc (CInt 0L) )) )) )) )) ))
             ; no_loc (Ret (Some (no_loc (Id "x")))) ] }) ]

let easy_p5_ast =
  [ Gvdecl (no_loc {name= "i"; init= no_loc (CInt 19L)})
  ; Gvdecl (no_loc {name= "b1"; init= no_loc (CBool true)})
  ; Gvdecl (no_loc {name= "b2"; init= no_loc (CBool false)})
  ; Gvdecl (no_loc {name= "str"; init= no_loc (CStr "This is a string!")})
  ; Gvdecl
      (no_loc
         { name= "arr1"
         ; init= no_loc (CArr (TInt, [no_loc (CInt 0L); no_loc (CInt 1L); no_loc (CInt 2L)])) })
  ; Gvdecl
      (no_loc
         { name= "arr2"
         ; init=
             no_loc
               (CArr
                  ( TRef (RArray TInt)
                  , [ no_loc (CArr (TInt, [no_loc (CInt 10L); no_loc (CInt 11L)]))
                    ; no_loc (CArr (TInt, [no_loc (CInt 20L); no_loc (CInt 21L)]))
                    ; no_loc (CArr (TInt, [no_loc (CInt 30L); no_loc (CInt 31L)])) ] )) })
  ; Gvdecl
      (no_loc
         { name= "arr3"
         ; init=
             no_loc
               (CArr
                  ( TRef RString
                  , [no_loc (CStr "String1"); no_loc (CStr "String2"); no_loc (CStr "String3")] ))
         })
  ; Gvdecl
      (no_loc
         { name= "arr4"
         ; init=
             no_loc
               (CArr
                  ( TRef (RArray (TRef RString))
                  , [ no_loc
                        (CArr (TRef RString, [no_loc (CStr "String00"); no_loc (CStr "String01")]))
                    ; no_loc
                        (CArr (TRef RString, [no_loc (CStr "String10"); no_loc (CStr "String11")]))
                    ; no_loc
                        (CArr (TRef RString, [no_loc (CStr "String20"); no_loc (CStr "String21")]))
                    ] )) }) ]

let easy_p6_ast =
  [ Gvdecl (no_loc {name= "y"; init= no_loc (CInt 0L)})
  ; Gvdecl (no_loc {name= "z"; init= no_loc (CInt 0L)})
  ; Gfdecl
      (no_loc
         { frtyp= RetVoid
         ; fname= "f"
         ; args= [(TInt, "x"); (TInt, "y")]
         ; body= [no_loc (Decl ("x", no_loc (CInt 0L))); no_loc (Ret None)] })
  ; Gfdecl
      (no_loc
         { frtyp= RetVoid
         ; fname= "g"
         ; args= [(TInt, "x"); (TInt, "y")]
         ; body= [no_loc (Decl ("z", no_loc (CInt 0L))); no_loc (Ret None)] }) ]

let easy_p7_ast =
  [ Gvdecl
      { elt=
          { name= "j"
          ; init=
              { elt=
                  CArr
                    ( TInt
                    , [ {elt= CInt 1L; loc= ("", (1, 17), (1, 18))}
                      ; {elt= CInt 2L; loc= ("", (1, 19), (1, 20))}
                      ; {elt= CInt 3L; loc= ("", (1, 21), (1, 22))}
                      ; {elt= CInt 4L; loc= ("", (1, 23), (1, 24))} ] )
              ; loc= ("", (1, 11), (1, 25)) } }
      ; loc= ("", (1, 0), (1, 26)) }
  ; Gfdecl
      { elt=
          { frtyp= RetVal (TRef (RArray TInt))
          ; fname= "f"
          ; args= []
          ; body=
              [ { elt=
                    Decl
                      ( "a"
                      , { elt=
                            CArr
                              ( TRef (RArray TInt)
                              , [ {elt= CInt 1L; loc= ("", (3, 22), (3, 23))}
                                ; {elt= CInt 2L; loc= ("", (3, 25), (3, 26))} ] )
                        ; loc= ("", (3, 10), (3, 27)) } )
                ; loc= ("", (3, 2), (3, 28)) }
              ; { elt=
                    Decl
                      ( "i"
                      , { elt= NewArr (TInt, {elt= CInt 4L; loc= ("", (4, 18), (4, 19))})
                        ; loc= ("", (4, 10), (4, 20)) } )
                ; loc= ("", (4, 2), (4, 21)) }
              ; { elt=
                    Decl
                      ( "arr1"
                      , { elt= NewArr (TInt, {elt= CInt 3L; loc= ("", (5, 21), (5, 22))})
                        ; loc= ("", (5, 13), (5, 23)) } )
                ; loc= ("", (5, 2), (5, 24)) }
              ; { elt=
                    Decl
                      ( "arr2"
                      , { elt=
                            NewArr (TRef (RArray TInt), {elt= CInt 3L; loc= ("", (6, 23), (6, 24))})
                        ; loc= ("", (6, 13), (6, 25)) } )
                ; loc= ("", (6, 2), (6, 26)) }
              ; { elt=
                    Ret
                      (Some
                         { elt= NewArr (TInt, {elt= CInt 2L; loc= ("", (7, 17), (7, 18))})
                         ; loc= ("", (7, 9), (7, 19)) })
                ; loc= ("", (7, 2), (7, 20)) } ] }
      ; loc= ("", (2, 0), (8, 1)) } ]

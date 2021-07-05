Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "blobs"
                }
            , alias = Nothing
            }
        , columns = Just
            ( "rowid" :|
                [ "k"
                , "v"
                ]
            )
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Values
                        ( Values
                            (
                                ( Expression'LiteralValue
                                    ( Number "3" ) :|
                                    [ Expression'LiteralValue
                                        ( String "three" )
                                    , Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "str"
                                            }
                                        )
                                    ]
                                ) :| []
                            )
                        )
                    )
                , orderBy = Nothing
                , limit = Nothing
                }
            ) Nothing
        , returning = Nothing
        }
    )
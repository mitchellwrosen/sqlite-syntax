Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "test2"
                }
            , alias = Nothing
            }
        , columns = Just
            ( "f1" :|
                [ "f2"
                , "f4"
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
                                    ( Number "77" ) :|
                                    [ Expression'LiteralValue
                                        ( Number "1.23" )
                                    , Expression'LiteralValue
                                        ( Number "3.45" )
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
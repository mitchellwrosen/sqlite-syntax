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
            ( "k" :|
                [ "v"
                , "i"
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
                                    ( Number "123" ) :|
                                    [ Expression'LiteralValue
                                        ( Number "567.765" )
                                    , Expression'LiteralValue Null
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
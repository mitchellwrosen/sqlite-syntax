Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "test1"
                }
            , alias = Nothing
            }
        , columns = Nothing
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Values
                        ( Values
                            (
                                ( Expression'LiteralValue
                                    ( Number "1" ) :|
                                    [ Expression'LiteralValue
                                        ( Number "2" )
                                    , Expression'LiteralValue
                                        ( Number "1.1" )
                                    , Expression'LiteralValue
                                        ( Number "2.2" )
                                    , Expression'LiteralValue
                                        ( String "hello" )
                                    , Expression'LiteralValue
                                        ( String "world" )
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
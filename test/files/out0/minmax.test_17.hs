Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "t2"
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
                                ( Expression'Plus
                                    ( Expression'FunctionCall
                                        ( FunctionCallExpression
                                            { call = FunctionCall
                                                { name = Namespaced
                                                    { namespace = Nothing
                                                    , value = "max_a_t2"
                                                    }
                                                , arguments = FunctionArguments'Arguments []
                                                }
                                            , filter = Nothing
                                            , over = Nothing
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "1" )
                                    ) :|
                                    [ Expression'LiteralValue
                                        ( Number "999" )
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
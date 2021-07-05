Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "t1"
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
                                    ( Number "3" ) :|
                                    [ Expression'LiteralValue
                                        ( Number "4" )
                                    , Expression'FunctionCall
                                        ( FunctionCallExpression
                                            { call = FunctionCall
                                                { name = Namespaced
                                                    { namespace = Nothing
                                                    , value = "zeroblob"
                                                    }
                                                , arguments = FunctionArguments'Arguments
                                                    [ Expression'LiteralValue
                                                        ( Number "10000" )
                                                    ]
                                                }
                                            , filter = Nothing
                                            , over = Nothing
                                            }
                                        )
                                    , Expression'LiteralValue
                                        ( Number "5" )
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
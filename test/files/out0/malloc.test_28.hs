Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "abc"
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
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "randstr"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'LiteralValue
                                                    ( Number "100" )
                                                , Expression'LiteralValue
                                                    ( Number "100" )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    ) :|
                                    [ Expression'FunctionCall
                                        ( FunctionCallExpression
                                            { call = FunctionCall
                                                { name = Namespaced
                                                    { namespace = Nothing
                                                    , value = "randstr"
                                                    }
                                                , arguments = FunctionArguments'Arguments
                                                    [ Expression'LiteralValue
                                                        ( Number "1000" )
                                                    , Expression'LiteralValue
                                                        ( Number "1000" )
                                                    ]
                                                }
                                            , filter = Nothing
                                            , over = Nothing
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
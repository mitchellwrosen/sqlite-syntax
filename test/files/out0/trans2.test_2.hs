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
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "id"
                                        }
                                    ) :|
                                    [ Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "u1"
                                            }
                                        )
                                    , Expression'FunctionCall
                                        ( FunctionCallExpression
                                            { call = FunctionCall
                                                { name = Namespaced
                                                    { namespace = Nothing
                                                    , value = "zeroblob"
                                                    }
                                                , arguments = FunctionArguments'Arguments
                                                    [ Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "z"
                                                            }
                                                        )
                                                    ]
                                                }
                                            , filter = Nothing
                                            , over = Nothing
                                            }
                                        )
                                    , Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "u2"
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
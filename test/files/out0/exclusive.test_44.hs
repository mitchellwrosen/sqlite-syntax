Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "t3"
                }
            , alias = Nothing
            }
        , columns = Nothing
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Select
                        ( Select
                            { distinct = False
                            , columns = ResultColumn'Expression
                                ( Aliased
                                    { value = Expression'FunctionCall
                                        ( FunctionCallExpression
                                            { call = FunctionCall
                                                { name = Namespaced
                                                    { namespace = Nothing
                                                    , value = "randstr"
                                                    }
                                                , arguments = FunctionArguments'Arguments
                                                    [ Expression'LiteralValue
                                                        ( Number "10" )
                                                    , Expression'LiteralValue
                                                        ( Number "400" )
                                                    ]
                                                }
                                            , filter = Nothing
                                            , over = Nothing
                                            }
                                        )
                                    , alias = Nothing
                                    }
                                ) :| []
                            , from = Just
                                ( Table
                                    ( QualifiedTableName
                                        { name = Aliased
                                            { value = Namespaced
                                                { namespace = Nothing
                                                , value = "t3"
                                                }
                                            , alias = Nothing
                                            }
                                        , indexedBy = Nothing
                                        }
                                    )
                                )
                            , where_ = Just
                                ( Expression'Equals
                                    ( Expression'Modulo
                                        ( Expression'FunctionCall
                                            ( FunctionCallExpression
                                                { call = FunctionCall
                                                    { name = Namespaced
                                                        { namespace = Nothing
                                                        , value = "random"
                                                        }
                                                    , arguments = FunctionArguments'Arguments []
                                                    }
                                                , filter = Nothing
                                                , over = Nothing
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "10" )
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "0" )
                                    )
                                )
                            , groupBy = Nothing
                            , window = Nothing
                            }
                        )
                    )
                , orderBy = Nothing
                , limit = Nothing
                }
            ) Nothing
        , returning = Nothing
        }
    )
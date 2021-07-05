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
                                                    , value = "a_string"
                                                    }
                                                , arguments = FunctionArguments'Arguments
                                                    [ Expression'LiteralValue
                                                        ( Number "300" )
                                                    ]
                                                }
                                            , filter = Nothing
                                            , over = Nothing
                                            }
                                        )
                                    , alias = Nothing
                                    }
                                ) :|
                                [ ResultColumn'Expression
                                    ( Aliased
                                        { value = Expression'FunctionCall
                                            ( FunctionCallExpression
                                                { call = FunctionCall
                                                    { name = Namespaced
                                                        { namespace = Nothing
                                                        , value = "a_string"
                                                        }
                                                    , arguments = FunctionArguments'Arguments
                                                        [ Expression'LiteralValue
                                                            ( Number "500" )
                                                        ]
                                                    }
                                                , filter = Nothing
                                                , over = Nothing
                                                }
                                            )
                                        , alias = Nothing
                                        }
                                    )
                                ]
                            , from = Just
                                ( Table
                                    ( QualifiedTableName
                                        { name = Aliased
                                            { value = Namespaced
                                                { namespace = Nothing
                                                , value = "t1"
                                                }
                                            , alias = Nothing
                                            }
                                        , indexedBy = Nothing
                                        }
                                    )
                                )
                            , where_ = Nothing
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
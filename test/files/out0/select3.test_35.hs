Statement'Select
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
                                            , value = "typeof"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "sum"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "a3"
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
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
                                        , value = "a"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Nothing
                    , groupBy = Just
                        ( GroupBy
                            { groupBy = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "a1"
                                    }
                                ) :| []
                            , having = Nothing
                            }
                        )
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )
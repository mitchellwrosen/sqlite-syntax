Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = True
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "artist"
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
                                                , value = "sum"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "timesplayed"
                                                        }
                                                    )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                , alias = Just "total"
                                }
                            )
                        ]
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "songs"
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
                            { groupBy = Expression'FunctionCall
                                ( FunctionCallExpression
                                    { call = FunctionCall
                                        { name = Namespaced
                                            { namespace = Nothing
                                            , value = "LOWER"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "artist"
                                                    }
                                                )
                                            ]
                                        }
                                    , filter = Nothing
                                    , over = Nothing
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
        , limit = Just
            ( Limit
                { limit = Expression'LiteralValue
                    ( Number "1" )
                , offset = Just
                    ( Expression'LiteralValue
                        ( Number "1" )
                    )
                }
            )
        }
    )
Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "log"
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
                                                , value = "count"
                                                }
                                            , arguments = FunctionArguments'Wildcard
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "avg"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "n"
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
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "max"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Plus
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "n"
                                                            }
                                                        )
                                                    )
                                                    ( Expression'Multiply
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "log"
                                                                }
                                                            )
                                                        )
                                                        ( Expression'LiteralValue
                                                            ( Number "2" )
                                                        )
                                                    )
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
                    , groupBy = Just
                        ( GroupBy
                            { groupBy = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "log"
                                    }
                                ) :| []
                            , having = Nothing
                            }
                        )
                    , window = Nothing
                    }
                )
            )
        , orderBy = Just
            ( OrderingTerm
                { expression = Expression'Plus
                    ( Expression'FunctionCall
                        ( FunctionCallExpression
                            { call = FunctionCall
                                { name = Namespaced
                                    { namespace = Nothing
                                    , value = "max"
                                    }
                                , arguments = FunctionArguments'Arguments
                                    [ Expression'Plus
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "n"
                                                }
                                            )
                                        )
                                        ( Expression'Multiply
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "log"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "2" )
                                            )
                                        )
                                    ]
                                }
                            , filter = Nothing
                            , over = Nothing
                            }
                        )
                    )
                    ( Expression'LiteralValue
                        ( Number "0" )
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :|
                [ OrderingTerm
                    { expression = Expression'Plus
                        ( Expression'FunctionCall
                            ( FunctionCallExpression
                                { call = FunctionCall
                                    { name = Namespaced
                                        { namespace = Nothing
                                        , value = "min"
                                        }
                                    , arguments = FunctionArguments'Arguments
                                        [ Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "log"
                                                }
                                            )
                                        , Expression'FunctionCall
                                            ( FunctionCallExpression
                                                { call = FunctionCall
                                                    { name = Namespaced
                                                        { namespace = Nothing
                                                        , value = "avg"
                                                        }
                                                    , arguments = FunctionArguments'Arguments
                                                        [ Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "n"
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
                        )
                        ( Expression'LiteralValue
                            ( Number "0" )
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                ]
            )
        , limit = Nothing
        }
    )
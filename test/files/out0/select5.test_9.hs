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
                                    , value = "y"
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
                                    , value = "y"
                                    }
                                ) :| []
                            , having = Just
                                ( Expression'LessThan
                                    ( Expression'FunctionCall
                                        ( FunctionCallExpression
                                            { call = FunctionCall
                                                { name = Namespaced
                                                    { namespace = Nothing
                                                    , value = "z"
                                                    }
                                                , arguments = FunctionArguments'Arguments
                                                    [ Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "y"
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
                                        ( Number "3" )
                                    )
                                )
                            }
                        )
                    , window = Nothing
                    }
                )
            )
        , orderBy = Just
            ( OrderingTerm
                { expression = Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "y"
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :| []
            )
        , limit = Nothing
        }
    )
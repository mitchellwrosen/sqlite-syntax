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
                                    , value = "count(*)"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "y"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
                        ( Table'Subquery
                            ( Aliased
                                { value = SelectStatement
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
                                                    ) :|
                                                    [ ResultColumn'Expression
                                                        ( Aliased
                                                            { value = Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "y"
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
                                , alias = Nothing
                                }
                            )
                        )
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Just
            ( OrderingTerm
                { expression = Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "count(*)"
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
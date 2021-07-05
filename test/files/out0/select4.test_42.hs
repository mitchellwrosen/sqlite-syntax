Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = Union
            ( CompoundSelect
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
                                    , alias = Just "cnt"
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
            )
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
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "n"
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
                    , where_ = Just
                        ( Expression'Equals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "n"
                                    }
                                )
                            )
                            ( Expression'LiteralValue
                                ( Number "7" )
                            )
                        )
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
                        , value = "cnt"
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :|
                [ OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "log"
                            }
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
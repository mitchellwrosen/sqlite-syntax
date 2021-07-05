Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Wildcard
                        ( Namespaced
                            { namespace = Nothing
                            , value = ()
                            }
                        ) :| []
                    , from = Just
                        ( Table'InnerJoin
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
                                    , alias = Just "a"
                                    }
                                )
                            )
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
                                                                            , value = "max"
                                                                            }
                                                                        , arguments = FunctionArguments'Arguments
                                                                            [ Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "x"
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
                                    , alias = Just "b"
                                    }
                                )
                            ) Nothing
                        )
                    , where_ = Just
                        ( Expression'Equals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "a"
                                            }
                                        )
                                    , value = "y"
                                    }
                                )
                            )
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    , value = "y"
                                    }
                                )
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
                        { namespace = Just
                            ( Namespaced
                                { namespace = Nothing
                                , value = "a"
                                }
                            )
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
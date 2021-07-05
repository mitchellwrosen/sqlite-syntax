Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Plus
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
                                ( Expression'LiteralValue
                                    ( Number "1" )
                                )
                            , alias = Just "x"
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
                                , alias = Just "y"
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
                                    , value = "x"
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
                { expression = Expression'Minus
                    ( Expression'LiteralValue
                        ( Number "10" )
                    )
                    ( Expression'Plus
                        ( Expression'Column
                            ( Namespaced
                                { namespace = Nothing
                                , value = "x"
                                }
                            )
                        )
                        ( Expression'Column
                            ( Namespaced
                                { namespace = Nothing
                                , value = "y"
                                }
                            )
                        )
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :| []
            )
        , limit = Nothing
        }
    )
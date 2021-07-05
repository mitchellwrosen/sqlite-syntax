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
                                    , value = "x"
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
                                                , value = "sequence"
                                                }
                                            , arguments = FunctionArguments'Arguments []
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
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Just
            ( OrderingTerm
                { expression = Expression'Modulo
                    ( Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "x"
                            }
                        )
                    )
                    ( Expression'LiteralValue
                        ( Number "2" )
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :|
                [ OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "y"
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
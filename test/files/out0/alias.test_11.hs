Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseAnd
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "random"
                                                }
                                            , arguments = FunctionArguments'Arguments []
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "2147483647" )
                                )
                            , alias = Just "r"
                            }
                        ) :| []
                    , from = Just
                        ( Table'InnerJoin
                            ( Table'InnerJoin
                                ( Table'InnerJoin
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
                                    ) Nothing
                                )
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
                                ) Nothing
                            )
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
                            ) Nothing
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
                        , value = "r"
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
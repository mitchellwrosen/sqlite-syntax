Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Minus
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "f1"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "23" )
                                )
                            , alias = Just "x"
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "test1"
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
                { expression = Expression'FunctionCall
                    ( FunctionCallExpression
                        { call = FunctionCall
                            { name = Namespaced
                                { namespace = Nothing
                                , value = "abs"
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
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :| []
            )
        , limit = Nothing
        }
    )
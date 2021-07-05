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
                                    , value = "t1"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "tbl1"
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
                                , value = "substr"
                                }
                            , arguments = FunctionArguments'Arguments
                                [ Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "t1"
                                        }
                                    )
                                , Expression'LiteralValue
                                    ( Number "2" )
                                , Expression'LiteralValue
                                    ( Number "20" )
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